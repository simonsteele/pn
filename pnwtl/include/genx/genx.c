/*
 * Copyright (c) 2004 by Tim Bray and Sun Microsystems.  For copying
 *  permission, see http://www.tbray.org/ongoing/genx/COPYING
 */

#define GENX_VERSION "beta3"

#include <stdlib.h>
#include <string.h>

#include "genx.h"

/*******************************
 * writer state
 */
typedef enum
{
  SEQUENCE_NO_DOC,
  SEQUENCE_PRE_DOC,
  SEQUENCE_POST_DOC,
  SEQUENCE_START_TAG,
  SEQUENCE_CONTENT
} writerSequence;

/*******************************
 * generic pointer list
 */
typedef struct
{
  genxWriter writer;
  int        count;
  int        space;
  void * *   pointers;
} plist;

/*******************************
 * text collector, for attribute values
 */
typedef struct
{
  utf8 buf;
  int  used;
  int  space;
} collector;

/*******************************
 * Structs with opaquely-exposed handles
 */
struct genxNamespace_rec
{
  genxWriter 	writer;
  utf8       	name;
  utf8       	prefix;
  genxAttribute declaration;
  int           inEffect;
};

struct genxElement_rec
{
  genxWriter 	writer;
  utf8       	type;
  genxNamespace ns;
};

typedef enum
{
  ATTR_NSDECL,
  ATTR_NAKED,
  ATTR_PREFIXED 
} attrType;

struct genxAttribute_rec
{
  genxWriter 	writer;
  utf8       	name;
  genxNamespace ns;
  collector     value;
  int           provided;   /* provided for current element? */
  attrType      atype;
};

/*******************************
 * genx's sandbox
 */
struct genxWriter_rec
{
  FILE *       	  	   file;
  genxSender *    	   sender;
  genxStatus   	  	   status;
  writerSequence  	   sequence;
  char            	   xmlChars[0x10000];
  void *          	   userData;
  plist           	   namespaces;
  int             	   nextPrefix;
  plist           	   elements;
  plist           	   attributes;
  plist           	   stack;   
  struct genxAttribute_rec arec;
  genxAttribute            defaultNS;
  char *                   etext[100];
  void *       		(* alloc)(void * userData, int bytes);
  void         		(* dealloc)(void * userData, void * data);
};

/*******************************
 * Forward declarations
 */
static genxAttribute declareAttribute(genxWriter w, genxNamespace ns,
				      utf8 name, utf8 value,
				      genxStatus * statusP);
static genxStatus addAttribute(genxAttribute a, const utf8 valuestr);
void genxSetCharProps(char * p);

/*******************************
 * End of declarations
 */

/*******************************
 * private memory utilities
 */
static void * allocate(genxWriter w, int bytes)
{
  if (w->alloc)
    return (void *) (*w->alloc)(w->userData, bytes);
  else
    return (void *) malloc(bytes);
}

static void deallocate(genxWriter w, void * data)
{
  if (w->dealloc)
    (*w->dealloc)(w->userData, data);
  else if (w->alloc == NULL)
    free(data);
}

static utf8 copy(genxWriter w, utf8 from)
{
  utf8 temp;

  if ((temp = (utf8) allocate(w, strlen((const char *) from) + 1)) == NULL)
    return NULL;
  strcpy((char *) temp, (const char *) from);
  return temp;
}

static genxStatus initCollector(genxWriter w, collector * c)
{
  c->space = 100;
  if ((c->buf = (utf8) allocate(w, c->space)) == NULL)
    return GENX_ALLOC_FAILED;
  c->used = 0;
  return GENX_SUCCESS;
}

static genxStatus growCollector(genxWriter w, collector * c, int size)
{
  utf8 newSpace; 

  c->space = size * 2;
  if ((newSpace = (utf8) allocate(w, c->space)) == NULL)
    return GENX_ALLOC_FAILED;

  strncpy((char *) newSpace, (const char *) c->buf, c->used);
  newSpace[c->used] = 0;
  deallocate(w, c->buf);
  c->buf = newSpace;
  return GENX_SUCCESS;
}

static void startCollect(collector * c)
{
  c->used = 0;
}
static void endCollect(collector * c)
{
  c->buf[c->used] = 0;
}

static genxStatus collectString(genxWriter w, collector * c, utf8 string)
{
  int sl = strlen((const char *) string);

  if (sl >= c->space)
    if ((w->status = growCollector(w, c, sl)) != GENX_SUCCESS)
      return GENX_ALLOC_FAILED;

  strcpy((char *) c->buf, (const char *) string);
  return GENX_SUCCESS;
}

#define collectPiece(w,c,d,size) {if (((c)->used+(size))>=(c)->space){if (((w)->status=growCollector(w,c,(c)->used+(size)))!=GENX_SUCCESS) return (w)->status;}strncpy((char *)(c)->buf+(c)->used,d,size);(c)->used+=size;}

/*******************************
 * private list utilities
 */
static genxStatus initPlist(genxWriter w, plist * pl)
{
  pl->writer = w;
  pl->count = 0;
  pl->space = 10;
  pl->pointers = (void * *) allocate(w, pl->space * sizeof(void *));
  if (pl->pointers == NULL)
    return GENX_ALLOC_FAILED;
  
  return GENX_SUCCESS;
}

/*
 * make room in a plist
 */
static int checkExpand(plist * pl)
{
  void * * newlist;
  int i;

  if (pl->count < pl->space)
    return 1;

  pl->space *= 2;
  newlist = (void * *) allocate(pl->writer, pl->space * sizeof(void *));
  if (newlist == NULL)
    return 0;
  for (i = 0; i < pl->count; i++)
    newlist[i] = pl->pointers[i];
  deallocate(pl->writer, pl->pointers); 
  pl->pointers = newlist;

  return 1;
}

/*
 * stick something on the end of a plist
 */
static genxStatus listAppend(plist * pl, void * pointer)
{
  if (!checkExpand(pl))
    return GENX_ALLOC_FAILED;

  pl->pointers[pl->count++] = pointer;
  return GENX_SUCCESS;
}

/*
 * insert in place, shuffling up
 */
static genxStatus listInsert(plist * pl, void * pointer, int at)
{
  int i;

  if (!checkExpand(pl))
    return GENX_ALLOC_FAILED;

  for (i = pl->count; i > at; i--)
    pl->pointers[i] = pl->pointers[i - 1];
  pl->count++;

  pl->pointers[at] = pointer;
  return GENX_SUCCESS;
}

/*******************************
 * list lookups
 */

static genxNamespace findNamespace(plist * pl, utf8 uri)
{
  int i;
  genxNamespace * nn = (genxNamespace *) pl->pointers;

  for (i = 0; i < pl->count; i++)
    if (strcmp((char *) uri, (const char *) nn[i]->name) == 0)
      return nn[i];

  return NULL;
}

static genxNamespace findNamespaceByPrefix(genxWriter w, utf8 prefix)
{
  int i;
  genxNamespace * nn = (genxNamespace *) w->namespaces.pointers;

  /*
   * we bypass the undeclare-default pattern
   */
  for (i = 0; i < w->namespaces.count; i++)
    if (strcmp((const char *) prefix, (const char *) nn[i]->prefix) == 0)
      return nn[i];

  return NULL;
}

static genxElement findElement(plist * pl, utf8 xmlns, utf8 type)
{
  int i;
  genxElement * ee = (genxElement *) pl->pointers;

  for (i = 0; i < pl->count; i++)
  {
    if (xmlns == NULL)
    {
      if (ee[i]->ns == NULL && strcmp((const char *) type,
				      (const char *) ee[i]->type) == 0)
	return ee[i];
    }
    else
    {
      if (ee[i]->ns != NULL &&
	  strcmp((const char *) xmlns, (const char *) ee[i]->ns->name) == 0 &&
	  strcmp((const char *) type, (const char *) ee[i]->type) == 0)
	return ee[i];
    }
  }

  return NULL;
}

/*******************************
 * UTF8 bit-banging
 */

/*
 * Retrieve the character pointed at, and advance the pointer; return -1 on
 *  error
 */
int genxNextUnicodeChar(utf8 * sp)
{
  utf8 s = *sp;
  int c;

  if (*s == 0)
    return -1;

  if (*s < 0x80)
    c = *s++;

  /* all this encoding sanity-checking taken from section 3.10 of Unicode 4 */
  else if (*s < 0xc2)
    goto malformed;

  /* 2-byte encodings, first byte c2 .. df */
  else if (*s < 0xe0) 
  {
    c = (*s++ & 0x1f) << 6;

    /*
     * for this common idiom, if ((c & 0xc0) != 0x80) is slightly faster
     *  on MacOS (PPC)
     */
    if (*s < 0x80 || *s > 0xbf)
      goto malformed;

    c |= *s++ & 0x3f;
  }

  /* 3-byte encodings, first byte e0 .. ef */
  else if (*s < 0xf0)
  {
    int b0 = *s;
    c = (*s++ & 0x0f) << 12;

    if ((b0 == 0xe0 && (*s < 0xa0 || *s > 0xbf)) ||
	(b0 <  0xed && (*s < 0x80 || *s > 0xbf)) ||
	(b0 == 0xed && (*s < 0x80 || *s > 0x9f)) ||
	(b0  > 0xed && (*s < 0x80 || *s > 0xbf)))
      goto malformed;

    c |= (*s++ & 0x3f) << 6;

    if (*s < 0x80 || *s > 0xbf)
      goto malformed;

    c |= *s++ & 0x3f;
  }

  /* 4-byte encodings, first byte f0 .. f4 */
  else if (*s < 0xf5)
  {
    int b0 = *s;
    c = (*s++ & 0x07) << 18;

    if ((b0 == 0xf0 && (*s < 0x90 || *s > 0xbf)) ||
	(b0 <  0xf4 && (*s < 0x80 || *s > 0xbf)) ||
	(b0 >= 0xf4 && (*s < 0x80 || *s > 0x8f)))
      goto malformed;

    c |= (*s++ & 0x3f) << 12;

    if (*s < 0x80 || *s > 0xbf)
      goto malformed;

    c |= (*s++ & 0x3f) << 6;

    if (*s < 0x80 || *s > 0xbf)
      goto malformed;

    c |= *s++ & 0x3f;
  }
  else
    goto malformed;

  *sp = s;
  return c;

  /*
   * this is needed by scrubText, which wants to get the pointer moved
   *  past the problem area.
   */
malformed:
  if (*s)
    ++s;
  *sp = s;
  return -1;
}

static int isXMLChar(genxWriter w, int c)
{
  if (c < 0)
    return 0;
  else if (c < 0x10000)
    return (int) w->xmlChars[c];
  else
    return (c <= 0x10ffff);
}

static int isLetter(genxWriter w, int c)
{
  if (c < 0 || c > 0xffff)
    return 0;
  else
    return w->xmlChars[c] & GENX_LETTER;
}

static int isNameChar(genxWriter w, int c)
{
  if (c < 0 || c > 0xffff)
    return 0;
  else
    return w->xmlChars[c] & GENX_NAMECHAR;
}

/*******************************
 * Constructors, setters/getters
 */

/*
 * Construct a new genxWriter
 */
genxWriter genxNew(void * (* alloc)(void * userData, int bytes),
		   void (* dealloc)(void * userData, void * data),
		   void * userData)
{
  genxWriter w;

  if (alloc)
    w = (genxWriter) (*alloc)(userData, sizeof(struct genxWriter_rec));
  else
    w = (genxWriter) malloc(sizeof(struct genxWriter_rec));

  if (w == NULL)
    return NULL;

  w->status = GENX_SUCCESS;
  w->alloc = alloc;
  w->dealloc = dealloc;
  w->userData = userData;
  w->sequence = SEQUENCE_NO_DOC;

  if (initPlist(w, &w->namespaces) != GENX_SUCCESS ||
      initPlist(w, &w->elements) != GENX_SUCCESS ||
      initPlist(w, &w->attributes) != GENX_SUCCESS ||
      initPlist(w, &w->stack) != GENX_SUCCESS)
    return NULL;

  if ((w->status = initCollector(w, &w->arec.value)) != GENX_SUCCESS)
    return NULL;

  w->nextPrefix = 1;

  w->defaultNS = declareAttribute(w, NULL,
				  (utf8) "xmlns", (utf8) "", &w->status);
  if (w->status != GENX_SUCCESS)
    return NULL;

  genxSetCharProps(w->xmlChars);

  w->etext[GENX_SUCCESS] = "Success";
  w->etext[GENX_BAD_UTF8] = "Bad UTF8";
  w->etext[GENX_NON_XML_CHARACTER] = "Non XML Character";
  w->etext[GENX_BAD_NAME] = "Bad NAME";
  w->etext[GENX_ALLOC_FAILED] = "Memory allocation failed";
  w->etext[GENX_BAD_NAMESPACE_NAME] = "Bad namespace name";
  w->etext[GENX_INTERNAL_ERROR] = "Internal error";
  w->etext[GENX_DUPLICATE_NAME] = "Duplicate name";
  w->etext[GENX_DUPLICATE_PREFIX] = "Duplicate prefix";
  w->etext[GENX_SEQUENCE_ERROR] = "Call out of sequence";
  w->etext[GENX_NO_START_TAG] = "No Start-tag for EndElement call";
  w->etext[GENX_IO_ERROR] = "I/O error";
  w->etext[GENX_PREMATURE_END] = "EndDocument call with unclosed element(s)";
  w->etext[GENX_MISSING_VALUE] = "Missing attribute value";
  w->etext[GENX_MALFORMED_COMMENT] = "Malformed comment body";
  w->etext[GENX_MALFORMED_PI] = "?> in PI";
  w->etext[GENX_XML_PI_TARGET] = "Target of PI matches [xX][mM][lL]";
  w->etext[GENX_DUPLICATE_ATTRIBUTE] =
    "Same attribute specified more than once";
  w->etext[GENX_ATTRIBUTE_IN_DEFAULT_NAMESPACE] =
    "Attribute cannot be in default namespace";
  w->etext[GENX_BAD_NAMESPACE_REDECLARATION] =
    "Attempt to redeclare namespace while still in scope";

  return w;
}

/*
 * get/set userData
 */
void genxSetUserData(genxWriter w, void * userData)
{
  w->userData = userData;
}
void * genxGetUserData(genxWriter w)
{
  return w->userData;
}

/*
 * get/set allocator
 */
void genxSetAlloc(genxWriter w, void * (* alloc)(void * userData, int bytes))
{
  w->alloc = alloc;
}
void genxSetDealloc(genxWriter w,
		    void (* dealloc)(void * userData, void * data))
{
  w->dealloc = dealloc;
}
void * (* genxGetAlloc(genxWriter w))(void * userData, int bytes)
{
  return w->alloc;
}
void (* genxGetDealloc(genxWriter w))(void * userData, void * data)
{
  return w->dealloc;
}

/*
 * Clean up
 */
void genxDispose(genxWriter w)
{
  int i;
  genxNamespace * nn = (genxNamespace *) w->namespaces.pointers;
  genxElement * ee = (genxElement *) w->elements.pointers;
  genxAttribute * aa = (genxAttribute *) w->attributes.pointers;

  for (i = 0; i < w->namespaces.count; i++)
  {
    deallocate(w, nn[i]->name);
    deallocate(w, nn[i]->prefix);
    deallocate(w, nn[i]);
  }

  for (i = 0; i < w->elements.count; i++)
  {
    deallocate(w, ee[i]->type);
    deallocate(w, ee[i]);
  }

  for (i = 0; i < w->attributes.count; i++)
  {
    deallocate(w, aa[i]->name);
    deallocate(w, aa[i]->value.buf);
    deallocate(w, aa[i]);
  }

  deallocate(w, w->namespaces.pointers);
  deallocate(w, w->elements.pointers);
  deallocate(w, w->attributes.pointers);
  deallocate(w, w->stack.pointers);

  deallocate(w, w->arec.value.buf);

  /* how Oscar dealt with Igli */
  deallocate(w, w);
}

/*******************************
 * External utility routines
 */

/*
 * scan a buffer and report problems with UTF-8 encoding or non-XML characters
 */
genxStatus genxCheckText(genxWriter w, const utf8 s)
{
  while (*s)
  {
    int c = genxNextUnicodeChar((utf8 *) &s);
    if (c == -1)
      return GENX_BAD_UTF8;

    if (!isXMLChar(w, c))
      return GENX_NON_XML_CHARACTER;
  }
  return GENX_SUCCESS;
}

/*
 * Purify some text
 */
int genxScrubText(genxWriter w, utf8 in, utf8 out)
{
  int problems = 0;
  utf8 last = in;

  while (*in)
  {
    int c = genxNextUnicodeChar(&in);
    if (c == -1)
    {
      problems++;
      last = in;
      continue;
    }

    if (!isXMLChar(w, c))
    {
      problems++;
      last = in;
      continue;
    }

    while (last < in)
      *out++ = *last++;
  }
  *out = 0;
  return problems;
}

/*
 * check one character
 */
int genxCharClass(genxWriter w, int c)
{
  int ret = 0;

  if (isXMLChar(w, c))
    ret |= GENX_XML_CHAR;
  if (isNameChar(w, c))
    ret |= GENX_NAMECHAR;
  if (isLetter(w, c))
    ret |= GENX_LETTER;
  return ret;
}

static genxStatus checkNCName(genxWriter w, utf8 name)
{
  int c;

  if (name == NULL || *name == 0)
    return GENX_BAD_NAME;

  c = genxNextUnicodeChar(&name);
  if (!isLetter(w, c))
    return GENX_BAD_NAME;

  while (*name)
  {
    c = genxNextUnicodeChar(&name);
    if (c == -1)
      return GENX_BAD_UTF8;
    if (!isNameChar(w, c))
      return GENX_BAD_NAME;
  }
  return GENX_SUCCESS;
}

char * genxGetErrorMessage(genxWriter w, genxStatus status)
{
  return w->etext[status];
}
char * genxLastErrorMessage(genxWriter w)
{
  return w->etext[w->status];
}

/*******************************
 * Declarations: namespace/element/attribute
 */

/*
 * DeclareNamespace - see genx.h for details
 */
genxNamespace genxDeclareNamespace(genxWriter w, utf8 uri, utf8 prefix,
				   genxStatus * statusP)
{
  genxNamespace old;
  genxNamespace ns;
  genxAttribute a;
  unsigned char nsdecl[1024];

  if (uri == NULL || uri[0] == 0)
  {
    w->status = *statusP = GENX_BAD_NAMESPACE_NAME;
    return NULL;
  }

  if ((w->status = genxCheckText(w, uri)) != GENX_SUCCESS)
  {
    *statusP = w->status;
    return NULL;
  }

  if (prefix != NULL && prefix[0] != 0)
  {
    if ((w->status = checkNCName(w, prefix)) != GENX_SUCCESS)
    {
      *statusP = w->status;
      return NULL;
    }
  }

  /* previously declared? */
  if ((old = findNamespace(&w->namespaces, uri)))
  {
    if (prefix && strcmp((const char *) old->prefix, (const char *) prefix))
    {
      if (old->inEffect)
      {
	w->status = *statusP = GENX_BAD_NAMESPACE_REDECLARATION;
	return NULL;
      }
	
      deallocate(w, old->prefix);
      old->prefix = copy(w, prefix);
      w->status = *statusP = GENX_SUCCESS;
      return old;
    }
    else
    {
      *statusP = GENX_SUCCESS;
      return old;
    }
  }

  /* this prefix currently used? */
  if (prefix && findNamespaceByPrefix(w, prefix))
  {
    w->status = *statusP = GENX_DUPLICATE_PREFIX;
    return NULL;
  }

  /* wasn't already declared */
  if (prefix == NULL)
  {
    /* make a prefix */
    unsigned char newPrefix[100];

    /* dodge malicious user having created their own g-%d prefix */
    do
      sprintf((char *) newPrefix, "g-%d", w->nextPrefix++);
    while (findNamespaceByPrefix(w, newPrefix));
      
    prefix = copy(w, newPrefix);
    if (prefix == NULL)
    {
      w->status = *statusP = GENX_ALLOC_FAILED;
      return NULL;
    }
  }
  else
  {
    utf8 temp;

    if ((temp = copy(w, prefix)) == NULL)
    {
      w->status = *statusP = GENX_ALLOC_FAILED;
      return NULL;
    }
    prefix = temp;
  }

  ns = (genxNamespace) allocate(w, sizeof(struct genxNamespace_rec));
  if (ns == NULL)
  {
    w->status = *statusP = GENX_ALLOC_FAILED;
    return NULL;
  }

  if ((ns->name = copy(w, uri)) == NULL)
  {
    w->status = *statusP = GENX_ALLOC_FAILED;
    return NULL;
  }

  ns->writer = w;
  ns->prefix = prefix;
  ns->inEffect = 0;
  if ((w->status = listAppend(&w->namespaces, ns)) != GENX_SUCCESS)
  {
    *statusP = w->status;
    return NULL;
  }

  if (ns->prefix[0] == 0)
    a = w->defaultNS;

  else
  {
    sprintf((char *) nsdecl, "xmlns:%s", (const char *) ns->prefix);

    a = declareAttribute(w, NULL, nsdecl, ns->name, statusP);
    if (a == NULL || *statusP != GENX_SUCCESS)
    {
      w->status = *statusP;
      return NULL;
    }
  }
  
  ns->declaration = a;
  
  *statusP = GENX_SUCCESS;
  return ns;
}

/*
 * get namespace prefix
 */
utf8 genxGetNamespacePrefix(genxNamespace ns)
{
  return ns->prefix;
}

/*
 * DeclareElement - see genx.h for details
 */
genxElement genxDeclareElement(genxWriter w,
			       genxNamespace ns, utf8 type, 
			       genxStatus * statusP)
{
  genxElement old;
  genxElement el;

  if ((w->status = checkNCName(w, type)) != GENX_SUCCESS)
  {
    *statusP = w->status;
    return NULL;
  }

  /* already declared? */
  if ((old = findElement(&w->elements, (ns == NULL) ? NULL : ns->name, type)))
    return old;

  if ((el = (genxElement) allocate(w, sizeof(struct genxElement_rec))) == NULL)
  {
    w->status = *statusP = GENX_ALLOC_FAILED;
    return NULL;
  }

  el->writer = w;
  el->ns = ns;
  if ((el->type = copy(w, type)) == NULL)
  {
    w->status = *statusP = GENX_ALLOC_FAILED;
    return NULL;
  }

  if ((w->status = listAppend(&w->elements, el)) != GENX_SUCCESS)
  {
    *statusP = w->status;
    return NULL;
  }
  
  *statusP = GENX_SUCCESS;
  return el;
}

/*
 * C14n ordering for attributes: 
 * - first, namespace declarations by the prefix being declared
 * - second, unprefixed attributes by attr name
 * - third, prefixed attrs by ns uri then local part
 */
static int orderAttributes(genxAttribute a1, genxAttribute a2)
{
  if (a1->atype == a2->atype)
  {
    if (a1->atype == ATTR_PREFIXED && a1->ns != a2->ns)
      return strcmp((const char *) a1->ns->name, (const char *) a2->ns->name);
    else
      return strcmp((const char *) a1->name, (const char *) a2->name);
  }

  else if (a1->atype == ATTR_NSDECL)
    return -1;

  else if (a1->atype == ATTR_NAKED)
  {
    if (a2->atype == ATTR_NSDECL)
      return 1;
    else
      return -1;
  }

  else
    return 1;
}

/*
 * internal declare-attribute allows specification of value, for
 *  use in declaring namespaces.
 * contains the first 'goto' statement I've written in 20 years
 */
static genxAttribute declareAttribute(genxWriter w, genxNamespace ns,
				      utf8 name, utf8 valuestr,
				      genxStatus * statusP)
{
  int high, low, probe;
  genxAttribute * aa = (genxAttribute *) w->attributes.pointers;
  genxAttribute a;

  w->arec.ns = ns;
  w->arec.name = name;
  if (valuestr)
  {
    w->status = collectString(w, &w->arec.value, valuestr);
    if (w->status != GENX_SUCCESS)
      goto busted;
    w->arec.atype = ATTR_NSDECL;
  }
  else if (ns)
  {
    /* no attributes in the default namespace */
    if (ns->prefix[0] == 0)
    {
      w->status = GENX_ATTRIBUTE_IN_DEFAULT_NAMESPACE;
      goto busted;
    }
    w->arec.atype = ATTR_PREFIXED;
  }
  else
    w->arec.atype = ATTR_NAKED;

  /* attribute list has to be kept sorted per c14n rules */
  high = w->attributes.count; low = -1;
  while (high - low > 1)
  {
    probe = (high + low) / 2;
    if (orderAttributes(&w->arec, aa[probe]) < 0)
      high = probe;
    else
      low = probe;
  }

  /* if it was already there */
  if (low != -1 && orderAttributes(&w->arec, aa[low]) == 0)
    return aa[low];

  /* not there, build it */
  a = (genxAttribute) allocate(w, sizeof(struct genxAttribute_rec));
  if (a == NULL)
  {
    w->status = GENX_ALLOC_FAILED;
    goto busted;
  }

  a->writer = w;
  a->ns = ns;
  a->provided = 0;
  a->atype = w->arec.atype;

  if ((a->name = copy(w, name)) == NULL)
  {
    w->status = GENX_ALLOC_FAILED;
    goto busted;
  }

  if ((w->status = initCollector(w, &a->value)) != GENX_SUCCESS)
    goto busted;

  if (valuestr)
    if ((w->status = collectString(w, &a->value, valuestr)) != GENX_SUCCESS)
      goto busted;
  
  w->status = listInsert(&w->attributes, a, high);
  if (w->status != GENX_SUCCESS)
    goto busted;
  
  *statusP = GENX_SUCCESS;
  return a;

busted:
  *statusP = w->status;
  return NULL;
}

/*
 * genxDeclareAttribute - see genx.h for details
 */
genxAttribute genxDeclareAttribute(genxWriter w,
				   genxNamespace ns, utf8 name,
				   genxStatus * statusP)
{
  if ((w->status = checkNCName(w, name)) != GENX_SUCCESS)
  {
    *statusP = w->status;
    return NULL;
  }

  return declareAttribute(w, ns, name, NULL, statusP);
}

/*******************************
 * I/O
 */
static genxStatus sendx(genxWriter w, utf8 s)
{
  if (w->sender)
    return (*w->sender->send)(w->userData, s);
  else
  {
    if (fputs((const char *) s, w->file) == -1)
      return GENX_IO_ERROR;
    else
      return GENX_SUCCESS;
  }
}

static genxStatus sendxBounded(genxWriter w, utf8 start, utf8 end)
{
  if (w->sender)
    return (*w->sender->sendBounded)(w->userData, start, end);
  else
    if (fwrite(start, 1, end - start, w->file) != end - start)
      return GENX_IO_ERROR;
    else
      return GENX_SUCCESS;
}

#define SendCheck(w,s) if ((w->status=sendx(w,(utf8)s))!=GENX_SUCCESS) return w->status;

/*******************************
 * XML writing routines.  The semantics of the externally-facing ones are
 *  written up in genx.h.  Commentary here is implementation notes and
 *  for internal routines.
 */

/*
 * Start a document
 */
genxStatus genxStartDocFile(genxWriter w, FILE * file)
{
  if (w->sequence != SEQUENCE_NO_DOC)
    return w->status = GENX_SEQUENCE_ERROR;
  
  w->sequence = SEQUENCE_PRE_DOC;
  w->file = file;
  w->sender = NULL;
  return GENX_SUCCESS;
}

genxStatus genxStartDocSender(genxWriter w, genxSender * sender)
{
  if (w->sequence != SEQUENCE_NO_DOC)
    return w->status = GENX_SEQUENCE_ERROR;
  
  w->sequence = SEQUENCE_PRE_DOC;
  w->file = NULL;
  w->sender = sender;
  return GENX_SUCCESS;
}

/*
 * Write out the attributes we've been gathering up for an element.  We save
 *  them until we've gathered them all so they can be writen in canonical
 *  order.
 * Also, we end the start-tag.
 * The trick here is that we keep the attribute list properly sorted as
 *  we build it, then as each attribute is added, we fill in its value and
 *  mark the fact that it's been added, in the "provided" field.
 */
static genxStatus writeAttributes(genxWriter w)
{
  int i;
  genxAttribute * aa = (genxAttribute *) w->attributes.pointers;

  for (i = 0; i < w->attributes.count; i++)
  {
    if (aa[i]->provided)
    {
      SendCheck(w, " ");

      if (aa[i]->ns)
      {
	SendCheck(w, aa[i]->ns->prefix)
	SendCheck(w, ":");
      }
      SendCheck(w, aa[i]->name);
      SendCheck(w, "=\"");
      SendCheck(w, aa[i]->value.buf);
      SendCheck(w, "\"");
    }
  }
  SendCheck(w, ">");
  return GENX_SUCCESS;
}

genxStatus genxStartElement(genxElement e)
{
  genxWriter w = e->writer;
  int i;

  switch (w->sequence)
  {
  case SEQUENCE_NO_DOC:
  case SEQUENCE_POST_DOC:
    return w->status = GENX_SEQUENCE_ERROR;
  case SEQUENCE_START_TAG:
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    break;
  case SEQUENCE_PRE_DOC:
  case SEQUENCE_CONTENT:
    break;
  }

  w->sequence = SEQUENCE_START_TAG;

  /* clear provided attributes */
  for (i = 0; i < w->attributes.count; i++)
    ((genxAttribute) w->attributes.pointers[i])->provided = 0;

  /*
   * push the stack.  We push a NULL after a pointer to this element,
   *  because the stack will also contain pointers to the namespace
   *  attributes that got declared here, so we can keep track of what's
   *  in effect.  I.e. a single stack entry consists logically of a pointer
   *  to an element object, a NULL, then zero or more pointers to
   *  namespace objects
   */
  if ((w->status = listAppend(&w->stack, e)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = listAppend(&w->stack, NULL)) != GENX_SUCCESS)
    return w->status;

  SendCheck(w, "<");
  if (e->ns)
  {
    if (e->ns->prefix[0])
    {
      SendCheck(w, e->ns->prefix);
      SendCheck(w, ":");
    }
    if ((w->status = genxAddNamespace(e->ns)) != GENX_SUCCESS)
      return w->status;
  }
  SendCheck(w, e->type);

  /*
   * if this is in no namespace, but there is a default namespace in
   *  effect, then we have to undeclare the default.  Fortunately,
   *  that sorts first per C14n rules.
   */
  if (e->ns == NULL)
  {
    int i;

    /*
     * could climb the stuack, but easier to just run through the namespaces
     */
    for (i = 0; i < w->namespaces.count; i++)
    {
      genxNamespace ns = (genxNamespace) w->namespaces.pointers[i];
      if (ns->inEffect && (ns->prefix[0] == 0))
      {
	if ((w->status = genxUnsetDefaultNamespace(w)) != GENX_SUCCESS)
	  return w->status;
      }
    }
  }

  return GENX_SUCCESS;
}

/*
 * clear the default namespace declaration
 */
genxStatus genxUnsetDefaultNamespace(genxWriter w)
{
  int i;
  genxNamespace * nn = (genxNamespace *) w->namespaces.pointers;

  for (i = 0; i < w->namespaces.count; i++)
    if (nn[i]->prefix[0] == 0)
    {
      nn[i]->inEffect = 0;
      break;
    }

  return genxAddAttribute(w->defaultNS, (utf8) "");
}

/*
 * Add a namespace declaration
 */
genxStatus genxAddNamespace(genxNamespace ns)
{
  genxWriter w = ns->writer;

  if (w->sequence != SEQUENCE_START_TAG)
    return w->status = GENX_SEQUENCE_ERROR;

  if (ns->inEffect)
    return GENX_SUCCESS;

  /*
   * If this is the default namespace, but the element is in no namespace,
   *  just refuse.  Have to look up the stack to find the element
   */
  if (ns->prefix[0] == 0)
  {
    int i;
    for (i = w->stack.count - 1; w->stack.pointers[i] != NULL; i--)
      ;
    --i;
    if (((genxElement) w->stack.pointers[i])->ns == NULL)
      return GENX_SUCCESS;
  }

  ns->inEffect = 1;
  if ((w->status = listAppend(&w->stack, ns)) != GENX_SUCCESS)
    return w->status;

  /*
   * if this is the default namespace, we have to plug in the URI value
   */
  if (ns->prefix[0] == 0)
    addAttribute(ns->declaration, ns->name);
  else
    addAttribute(ns->declaration, NULL);
  return GENX_SUCCESS;
}

/*
 * Private attribute-adding code
 * most of the work here is normalizing the value, which is the same
 *  as regular normalization except for " is replaced by "&quot;"
 */
static genxStatus addAttribute(genxAttribute a, const utf8 valuestr)
{
  utf8 lastv = valuestr;
  genxWriter w = a->writer;

  /* if valuestr not provided, this is an xmlns with a pre-cooked value */
  if (valuestr)
  {
    startCollect(&a->value);
    while (*valuestr)
    {
      int c = genxNextUnicodeChar((utf8 *) &valuestr);
      
      if (c == -1)
	return w->status = GENX_BAD_UTF8;
      
      if (!isXMLChar(w, c))
	return w->status = GENX_NON_XML_CHARACTER;
      
      switch(c)
      {
      case 0xd:
	collectPiece(w, &a->value, "&#xD;", 5); 
	break;
      case '"':
	collectPiece(w, &a->value, "&quot;", 6);
	break;
      case '<':
	collectPiece(w, &a->value, "&lt;", 4);
	break;
      case '&':
	collectPiece(w, &a->value, "&amp;", 5);
	break;
      case '>':
	collectPiece(w, &a->value, "&gt;", 4);
	break;
      default:
	collectPiece(w, &a->value, (const char *) lastv, valuestr - lastv);
	break;
      }
      lastv = valuestr;
    }
    endCollect(&a->value);
  }

  /* now add the namespace attribute */
  if (a->ns)
    if ((w->status = genxAddNamespace(a->ns)) != GENX_SUCCESS)
      return w->status;

  if (valuestr && a->provided)
    return w->status = GENX_DUPLICATE_ATTRIBUTE;
  a->provided = 1;

  return GENX_SUCCESS;
}

/*
 * public attribute adder.
 * The only difference is that it doesn't allow a NULL value
 */
genxStatus genxAddAttribute(genxAttribute a, const utf8 valuestr)
{
  if (a->writer->sequence != SEQUENCE_START_TAG)
    return a->writer->status = GENX_SEQUENCE_ERROR;

  if (valuestr == NULL)
    return a->writer->status = GENX_MISSING_VALUE;

  return addAttribute(a, valuestr);
}

genxStatus genxEndElement(genxWriter w)
{
  genxElement e;

  switch (w->sequence)
  {
  case SEQUENCE_NO_DOC:
  case SEQUENCE_PRE_DOC:
  case SEQUENCE_POST_DOC:
    return w->status = GENX_SEQUENCE_ERROR;
  case SEQUENCE_START_TAG:
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    break;
  case SEQUENCE_CONTENT:
    break;
  }

  /*
   * pop zero or more namespace declarations, then a null, then the
   *  start-element declaration off the stack
   */
  while (w->stack.count > 0 && w->stack.pointers[w->stack.count - 1] != NULL)
  {
    /* namespace record */
    genxNamespace ns = (genxNamespace) w->stack.pointers[--w->stack.count];
    ns->inEffect = 0;
  }

  /* pop the NULL */
  --w->stack.count;
  if (w->stack.count <= 0)
    return w->status = GENX_NO_START_TAG;

  e = (genxElement) w->stack.pointers[--w->stack.count];
  if (w->stack.count == 0)
    w->sequence = SEQUENCE_POST_DOC;
  else
    w->sequence = SEQUENCE_CONTENT;

  SendCheck(w, "</");
  if (e->ns && e->ns->prefix[0])
  {
    SendCheck(w, e->ns->prefix);
    SendCheck(w, ":");
  }
  SendCheck(w, e->type);
  SendCheck(w, ">");

  return GENX_SUCCESS;
}

/*
 * Internal character-adder.  It tries to keep the number of sendx()
 *  calls down by looking at each character but only doing the output
 *  when it has to escape something; ordinary text gets saved up in
 *  chunks the start of which is indicated by *breaker.
 * c is the character, next points to the UTF8 representing the next
 *  lastsP indirectly points to the UTF8 representing the
 *  character, breakerP* indirectly points to the last place genx
 *  changed the UTF8, e.g. by escaping a '<'
 */
static genxStatus addChar(genxWriter w, int c, utf8 next,
			  utf8 * lastsP, utf8 * breakerP)
{
  if (c == -1)
    return GENX_BAD_UTF8;

  if (!isXMLChar(w, c))
    return GENX_NON_XML_CHARACTER;

  switch(c)
  {
  case 0xd:
    if ((w->status = sendxBounded(w, *breakerP, *lastsP)) != GENX_SUCCESS)
      return w->status;
    *breakerP = next;
    sendx(w, (utf8) "&#xD;");
    break;
  case '<':
    if ((w->status = sendxBounded(w, *breakerP, *lastsP)) != GENX_SUCCESS)
      return w->status;
    *breakerP = next;
    sendx(w, (utf8) "&lt;");
    break;
  case '&':
    if ((w->status = sendxBounded(w, *breakerP, *lastsP)) != GENX_SUCCESS)
      return w->status;
    *breakerP = next;
    sendx(w, (utf8) "&amp;");
    break;
  case '>':
    if ((w->status = sendxBounded(w, *breakerP, *lastsP)) != GENX_SUCCESS)
      return w->status;
    *breakerP = next;
    sendx(w, (utf8) "&gt;");
    break;
  default:
    break;
  }
  *lastsP = next;
  return GENX_SUCCESS;
}

genxStatus genxAddText(genxWriter w, const utf8 start)
{
  utf8 lasts = start;
  utf8 breaker = start;

  if (w->sequence == SEQUENCE_START_TAG)
  {
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  if (w->sequence != SEQUENCE_CONTENT)
    return w->status = GENX_SEQUENCE_ERROR;

  while (*start)
  {
    int c = genxNextUnicodeChar((utf8 *) &start);

    if ((w->status = addChar(w, c, start, &lasts, &breaker)) != GENX_SUCCESS)
      return w->status;
  }
  return sendxBounded(w, breaker, start);
}

genxStatus genxAddBoundedText(genxWriter w, const utf8 start, const utf8 end)
{
  utf8 lasts = start;
  utf8 breaker = start;

  if (w->sequence == SEQUENCE_START_TAG)
  {
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  if (w->sequence != SEQUENCE_CONTENT)
    return w->status = GENX_SEQUENCE_ERROR;

  while (start < end)
  {
    int c = genxNextUnicodeChar((utf8 *) &start);

    if ((w->status = addChar(w, c, start, &lasts, &breaker)) != GENX_SUCCESS)
      return w->status;
  }
  return sendxBounded(w, breaker, start);
}

genxStatus genxAddCountedText(genxWriter w, const utf8 start, int byteCount)
{
  utf8 end = start + byteCount;

  return genxAddBoundedText(w, start, end);
}

genxStatus genxAddCharacter(genxWriter w, int c)
{
  unsigned char cUTF8[10];
  utf8 lasts, breaker, next;

  if (w->sequence == SEQUENCE_START_TAG)
  {
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  if (w->sequence != SEQUENCE_CONTENT)
    return w->status = GENX_SEQUENCE_ERROR;

  if (!isXMLChar(w, c))
    return w->status = GENX_NON_XML_CHARACTER;

  /* make UTF8 representation of character */
  lasts = breaker = next = cUTF8;

  if (c < 0x80)
    *next++ = c;
  else if (c < 0x800)
  {
    *next++ = 0xc0 | (c >> 6);
    *next++ = 0x80 | (c & 0x3f);
  }
  else if (c < 0x10000)
  {
    *next++ = 0xe0 | (c >> 12);
    *next++ = 0x80 | ((c & 0xfc0) >> 6);
    *next++ = 0x80 | (c & 0x3f);
  }
  else
  {
    *next++ = 0xf0 | (c >> 18);
    *next++ = 0x80 | ((c & 0x3f000) >> 12);
    *next++ = 0x80 | ((c & 0xfc0) >> 6);
    *next++ = 0x80 | (c & 0x3f);
  }
  *next = 0;

  if ((w->status = addChar(w, c, next, &lasts, &breaker)) != GENX_SUCCESS)
    return w->status;

  return sendxBounded(w, breaker, next);
}  

genxStatus genxEndDocument(genxWriter w)
{
  if (w->sequence != SEQUENCE_POST_DOC)
    return w->status = GENX_SEQUENCE_ERROR;

  if (w->file)
    fflush(w->file);
  else
    if ((w->status = (*w->sender->flush)(w->userData)) != GENX_SUCCESS)
      return w->status;

  w->sequence = SEQUENCE_NO_DOC;
  return GENX_SUCCESS;
}

genxStatus genxComment(genxWriter w, const utf8 text)
{
  int i;

  if (w->sequence == SEQUENCE_NO_DOC)
    return w->status = GENX_SEQUENCE_ERROR;

  if ((w->status = genxCheckText(w, text)) != GENX_SUCCESS)
    return w->status;

  /* no leading '-', no trailing '-', no '--' */
  if (text[0] == '-')
    return w->status = GENX_MALFORMED_COMMENT;
  for (i = 0; text[i]; i++)
    if (text[i] == '-' && (text[i + 1] == '-' || text[i + 1] == 0))
      return w->status = GENX_MALFORMED_COMMENT;

  if (w->sequence == SEQUENCE_START_TAG)
  {
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  else if (w->sequence == SEQUENCE_POST_DOC)
    if ((w->status = sendx(w, (utf8) "\n")) != GENX_SUCCESS)
      return w->status;

  if ((w->status = sendx(w, (utf8) "<!--")) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, text)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, (utf8) "-->")) != GENX_SUCCESS)
    return w->status;

  if (w->sequence == SEQUENCE_PRE_DOC)
    if ((w->status = sendx(w, (utf8) "\n")) != GENX_SUCCESS)
      return w->status;

  return GENX_SUCCESS;
}

genxStatus genxPI(genxWriter w, const utf8 target, const utf8 text)
{
  int i;

  if (w->sequence == SEQUENCE_NO_DOC)
    return w->status = GENX_SEQUENCE_ERROR;

  if ((w->status = genxCheckText(w, target)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = checkNCName(w, target)) != GENX_SUCCESS)
    return w->status;
  if ((strlen((const char *) target) >= 3) &&
      (target[0] == 'x' || target[0] == 'X') &&
      (target[1] == 'm' || target[1] == 'M') &&
      (target[2] == 'l' || target[2] == 'L') &&
      (target[3] == 0))
    return w->status = GENX_XML_PI_TARGET;

  if ((w->status = genxCheckText(w, text)) != GENX_SUCCESS)
    return w->status;

  /* no ?> within */
  for (i = 1; text[i]; i++)
    if (text[i] == '>' && text[i - 1] == '?')
      return w->status = GENX_MALFORMED_PI;

  if (w->sequence == SEQUENCE_START_TAG)
  {
    if ((w->status = writeAttributes(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  else if (w->sequence == SEQUENCE_POST_DOC)
    if ((w->status = sendx(w, (utf8) "\n")) != GENX_SUCCESS)
      return w->status;

  if ((w->status = sendx(w, (utf8) "<?")) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, target)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, (utf8) " ")) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, text)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, (utf8) "?>")) != GENX_SUCCESS)
    return w->status;

  if (w->sequence == SEQUENCE_PRE_DOC)
    if ((w->status = sendx(w, (utf8) "\n")) != GENX_SUCCESS)
      return w->status;

  return GENX_SUCCESS;
}

/*******************************
 * Literal versions of the writing routines
 */
genxStatus genxStartElementLiteral(genxWriter w,
				   const utf8 xmlns, const utf8 type)
{
  genxNamespace ns = NULL;
  genxElement e;

  if (xmlns)
  {
    ns = genxDeclareNamespace(w, xmlns, NULL, &w->status);
    if (ns == NULL || w->status != GENX_SUCCESS)
      return w->status;
  }
  e = genxDeclareElement(w, ns, type, &w->status);
  if (e == NULL || w->status != GENX_SUCCESS)
    return w->status;

  return genxStartElement(e);
}

genxStatus genxAddAttributeLiteral(genxWriter w, const utf8 xmlns,
				   const utf8 name, const utf8 value)
{
  genxNamespace ns = NULL;
  genxAttribute a;

  if (xmlns)
  {
    ns = genxDeclareNamespace(w, xmlns, NULL, &w->status);
    if (ns == NULL && w->status != GENX_SUCCESS)
      return w->status;
  }
  
  a = genxDeclareAttribute(w, ns, name, &w->status);
  if (a == NULL || w->status != GENX_SUCCESS)
    return w->status;

  return genxAddAttribute(a, value);
}

/*
 * return version
 */
char * genxGetVersion()
{
  return GENX_VERSION;
}
 
