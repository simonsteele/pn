/*
 * Copyright (c) 2004 by Tim Bray and Sun Microsystems.  For copying
 *  permission, see http://www.tbray.org/ongoing/genx/COPYING
 */

#define GENX_VERSION "beta5"

#include <stdlib.h>
#include <string.h>

#include "genx.h"

#define Boolean int
#define True 1
#define False 0
#define STRLEN_XMLNS_COLON 6


/*******************************
 * writer state
 */
typedef enum
{
  SEQUENCE_NO_DOC,
  SEQUENCE_PRE_DOC,
  SEQUENCE_POST_DOC,
  SEQUENCE_START_TAG,
  SEQUENCE_ATTRIBUTES,
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

/*
 * This one's tricky, to handle stacking namespaces
 *  'declaration' is the current attribute which would be used to
 *    declare the currently-effective prefix
 *  'defDeclaration' is a appropriate declaration when this is being
 *    used with the default prefix as passed to genxDeclareNamespace
 *  baroque is true if this namespace has been used with more than one
 *   prefix, or is the default namespace but has been unset
 */
struct genxNamespace_rec
{
  genxWriter 	writer;
  utf8       	name;
  int           declCount;
  Boolean       baroque;
  genxAttribute declaration;
  genxAttribute defaultDecl;
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
  int             	   nextPrefix;
  utf8                     empty;
  Boolean                  defaultNsDeclared;
  genxAttribute            xmlnsEquals;
  genxElement              nowStarting;
  plist           	   namespaces;
  plist           	   elements;
  plist           	   attributes;
  plist                    prefixes;
  plist           	   stack;   
  struct genxAttribute_rec arec;
  char *                   etext[100];
  void *       		(* alloc)(void * userData, int bytes);
  void         		(* dealloc)(void * userData, void * data);
};

/*******************************
 * Forward declarations
 */
static genxAttribute declareAttribute(genxWriter w, genxNamespace ns,
				      constUtf8 name, constUtf8 valuestr,
				      genxStatus * statusP);
static genxStatus addNamespace(genxNamespace ns, utf8 prefix);
static genxStatus unsetDefaultNamespace(genxWriter w);
static genxStatus addAttribute(genxAttribute a, constUtf8 valuestr);
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

static utf8 copy(genxWriter w, constUtf8 from)
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

static genxStatus collectString(genxWriter w, collector * c, constUtf8 string)
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
static Boolean checkExpand(plist * pl)
{
  void * * newlist;
  int i;

  if (pl->count < pl->space)
    return True;

  pl->space *= 2;
  newlist = (void * *) allocate(pl->writer, pl->space * sizeof(void *));
  if (newlist == NULL)
    return False;
  for (i = 0; i < pl->count; i++)
    newlist[i] = pl->pointers[i];
  deallocate(pl->writer, pl->pointers); 
  pl->pointers = newlist;

  return True;
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

static genxNamespace findNamespace(plist * pl, constUtf8 uri)
{
  int i;
  genxNamespace * nn = (genxNamespace *) pl->pointers;

  for (i = 0; i < pl->count; i++)
    if (strcmp((char *) uri, (const char *) nn[i]->name) == 0)
      return nn[i];

  return NULL;
}

static genxElement findElement(plist * pl, constUtf8 xmlns, constUtf8 type)
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

/*
 * store & intern a prefix, after giving it the
 *  "xmlns:" prefix.  Don't allow storing the same one twice unless 'force'
 *  is set.
 */
static utf8 storePrefix(genxWriter w, constUtf8 prefix, Boolean force)
{
  int high, low;
  utf8 * pp = (utf8 *) w->prefixes.pointers;
  unsigned char buf[1024];

  if (prefix[0] == 0)
    prefix = (utf8) "xmlns";
  else
  {
    sprintf((char *) buf, "xmlns:%s", prefix);
    prefix = buf;
  }

  high = w->prefixes.count; low = -1;
  while (high - low > 1)
  {
    int probe = (high + low) / 2;
    if (strcmp((const char *) prefix, (const char *) pp[probe]) < 0)
      high = probe;
    else
      low = probe;
  }

  /* already there? */
  if (low != -1 && strcmp((const char *) prefix, (const char *) pp[low]) == 0)
  {
    if (force)
      return pp[low];

    w->status = GENX_DUPLICATE_PREFIX;
    return NULL;
  }

  /* copy & insert */
  if ((prefix = copy(w, prefix)) == NULL)
  {
    w->status = GENX_ALLOC_FAILED;
    return NULL;
  }
  
  w->status = listInsert(&w->prefixes, (void *) prefix, high);
  if (w->status != GENX_SUCCESS)
    return NULL;

  return (utf8) prefix;
}

/*******************************
 * UTF8 bit-banging
 */

/*
 * Retrieve the character pointed at, and advance the pointer; return -1 on
 *  error
 */
int genxNextUnicodeChar(constUtf8 * sp)
{
  utf8 s = (utf8) *sp;
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

static Boolean isXMLChar(genxWriter w, int c)
{
  if (c < 0)
    return False;
  else if (c < 0x10000)
    return (int) w->xmlChars[c];
  else
    return (c <= 0x10ffff);
}

static Boolean isLetter(genxWriter w, int c)
{
  if (c < 0 || c > 0xffff)
    return False;
  else
    return w->xmlChars[c] & GENX_LETTER;
}

static Boolean isNameChar(genxWriter w, int c)
{
  if (c < 0 || c > 0xffff)
    return False;
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
  genxNamespace xml;

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
      initPlist(w, &w->prefixes) != GENX_SUCCESS ||
      initPlist(w, &w->stack) != GENX_SUCCESS)
    return NULL;

  if ((w->status = initCollector(w, &w->arec.value)) != GENX_SUCCESS)
    return NULL;

  if ((w->empty = copy(w, (utf8) "")) == NULL)
  {
    w->status = GENX_ALLOC_FAILED;
    return NULL;
  }

  w->xmlnsEquals = declareAttribute(w, NULL, (utf8) "xmlns", NULL, &w->status);
  if (w->xmlnsEquals == NULL || w->status != GENX_SUCCESS)
    return NULL;
  w->defaultNsDeclared = False;

  w->nextPrefix = 1;

  genxSetCharProps(w->xmlChars);

  w->etext[GENX_SUCCESS] = "Success";
  w->etext[GENX_BAD_UTF8] = "Bad UTF8";
  w->etext[GENX_NON_XML_CHARACTER] = "Non XML Character";
  w->etext[GENX_BAD_NAME] = "Bad NAME";
  w->etext[GENX_ALLOC_FAILED] = "Memory allocation failed";
  w->etext[GENX_BAD_NAMESPACE_NAME] = "Bad namespace name";
  w->etext[GENX_INTERNAL_ERROR] = "Internal error";
  w->etext[GENX_DUPLICATE_PREFIX] = "Duplicate prefix";
  w->etext[GENX_SEQUENCE_ERROR] = "Call out of sequence";
  w->etext[GENX_NO_START_TAG] = "No Start-tag for EndElement call";
  w->etext[GENX_IO_ERROR] = "I/O error";
  w->etext[GENX_MISSING_VALUE] = "Missing attribute value";
  w->etext[GENX_MALFORMED_COMMENT] = "Malformed comment body";
  w->etext[GENX_MALFORMED_PI] = "?> in PI";
  w->etext[GENX_XML_PI_TARGET] = "Target of PI matches [xX][mM][lL]";
  w->etext[GENX_DUPLICATE_ATTRIBUTE] =
    "Same attribute specified more than once";
  w->etext[GENX_ATTRIBUTE_IN_DEFAULT_NAMESPACE] =
    "Attribute cannot be in default namespace";
  w->etext[GENX_DUPLICATE_NAMESPACE] =
    "Declared namespace twice with different prefixes on one element.";
  w->etext[GENX_BAD_DEFAULT_DECLARATION] =
    "Declared a default namespace on an element which is in no namespace";

  /* the xml: namespace is pre-wired */
  xml = genxDeclareNamespace(w, (utf8) "http://www.w3.org/XML/1998/namespace",
			     (utf8) "xml", &w->status);
  if (xml == NULL)
    return NULL;
  xml->declCount = 1;
  xml->declaration = xml->defaultDecl;

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
  utf8 * pp = (utf8 *) w->prefixes.pointers;

  for (i = 0; i < w->namespaces.count; i++)
  {
    deallocate(w, nn[i]->name);
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

  for(i = 0; i < w->prefixes.count; i++)
  {
    deallocate(w, pp[i]);
  }

  deallocate(w, w->namespaces.pointers);
  deallocate(w, w->elements.pointers);
  deallocate(w, w->attributes.pointers);
  deallocate(w, w->prefixes.pointers);
  deallocate(w, w->stack.pointers);

  deallocate(w, w->arec.value.buf);

  deallocate(w, w->empty);

  /* how Oscar dealt with Igli */
  deallocate(w, w);
}

/*******************************
 * External utility routines
 */

/*
 * scan a buffer and report problems with UTF-8 encoding or non-XML characters
 */
genxStatus genxCheckText(genxWriter w, constUtf8 s)
{
  while (*s)
  {
    int c = genxNextUnicodeChar(&s);
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
int genxScrubText(genxWriter w, constUtf8 in, utf8 out)
{
  int problems = 0;
  constUtf8 last = in;

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

static genxStatus checkNCName(genxWriter w, constUtf8 name)
{
  int c;

  if (name == NULL || *name == 0)
    return GENX_BAD_NAME;

  c = genxNextUnicodeChar(&name);
  if (!isLetter(w, c) && c != ':' && c != '_')
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
 * DeclareNamespace - by far the most complex routine in Genx
 */
genxNamespace genxDeclareNamespace(genxWriter w, constUtf8 uri,
				   constUtf8 defaultPref,
				   genxStatus * statusP)
{
  genxNamespace ns;
  genxAttribute defaultDecl;
  unsigned char newPrefix[100];

  if (uri == NULL || uri[0] == 0)
  {
    w->status = GENX_BAD_NAMESPACE_NAME;
    goto busted;
  }

  if ((w->status = genxCheckText(w, uri)) != GENX_SUCCESS)
    goto busted;

  /* if a prefix is provided, it has to be an NCname */
  if (defaultPref != NULL && defaultPref[0] != 0 &&
      (w->status = checkNCName(w, defaultPref)) != GENX_SUCCESS)
    goto busted;

  /* previously declared? */
  if ((ns = findNamespace(&w->namespaces, uri)))
  {
    /* just a lookup, really */
    if ((defaultPref == NULL) ||
	(defaultPref[0] == 0 && ns->defaultDecl == w->xmlnsEquals) ||
	(strcmp((const char *) ns->defaultDecl->name + STRLEN_XMLNS_COLON,
		(const char *) defaultPref) == 0))
    {
      w->status = *statusP = GENX_SUCCESS;
      return ns;
    }
  }

  /* wasn't already declared */
  else
  {

    /* make a default prefix if none provided */
    if (defaultPref == NULL)
    {
      sprintf((char *) newPrefix, "g%d", w->nextPrefix++);
      defaultPref = newPrefix;
    }

    ns = (genxNamespace) allocate(w, sizeof(struct genxNamespace_rec));
    if (ns == NULL)
    {
      w->status = GENX_ALLOC_FAILED;
      goto busted;
    }
    ns->writer = w;
    ns->baroque = False;

    if ((ns->name = copy(w, uri)) == NULL)
    {
      w->status = GENX_ALLOC_FAILED;
      goto busted;
    }

    if ((w->status = listAppend(&w->namespaces, ns)) != GENX_SUCCESS)
      goto busted;
    ns->defaultDecl = ns->declaration = NULL;
    ns->declCount = 0;
  }

  if (defaultPref[0] == 0)
  {
    if (w->defaultNsDeclared)
    {
      w->status = GENX_DUPLICATE_PREFIX;
      goto busted;
    }
    defaultDecl = w->xmlnsEquals;
    w->defaultNsDeclared = True;
  }
  else
  {
    /* this catches dupes too */
    if ((defaultPref = storePrefix(w, defaultPref, False)) == NULL)
      goto busted;

    defaultDecl = declareAttribute(w, NULL, defaultPref, ns->name, statusP);
    if (defaultDecl == NULL || *statusP != GENX_SUCCESS)
    {
      w->status = *statusP;
      return NULL;
    }
  }

  if (ns->defaultDecl != NULL && defaultDecl != ns->defaultDecl)
    ns->baroque = True;
  ns->defaultDecl = defaultDecl;
  
  *statusP = GENX_SUCCESS;
  return ns;

busted:
  *statusP = w->status;
  return NULL;
}

/*
 * get namespace prefix
 */
utf8 genxGetNamespacePrefix(genxNamespace ns)
{
  if (ns->declaration == NULL)
    return NULL;

  if (ns->declaration == ns->writer->xmlnsEquals)
    return ns->writer->empty;

  return ns->declaration->name + STRLEN_XMLNS_COLON;
}

/*
 * DeclareElement - see genx.h for details
 */
genxElement genxDeclareElement(genxWriter w,
			       genxNamespace ns, constUtf8 type, 
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
  old = findElement(&w->elements, (ns == NULL) ? NULL : ns->name, type);
  if (old)
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
 * internal declare-attribute.  This one allows colonized values for
 *  names, so that you can declare xmlns:-type attributes
 */
static genxAttribute declareAttribute(genxWriter w, genxNamespace ns,
				      constUtf8 name, constUtf8 valuestr,
				      genxStatus * statusP)
{
  int high, low;
  genxAttribute * aa = (genxAttribute *) w->attributes.pointers;
  genxAttribute a;

  w->arec.ns = ns;
  w->arec.name = (utf8) name;

  if (ns)
    w->arec.atype = ATTR_PREFIXED;
  else if (strncmp((const char *) name, "xmlns", STRLEN_XMLNS_COLON - 1) == 0)
    w->arec.atype = ATTR_NSDECL;
  else
    w->arec.atype = ATTR_NAKED;

  if (ns && (ns->defaultDecl == w->xmlnsEquals))
  {
    w->status = GENX_ATTRIBUTE_IN_DEFAULT_NAMESPACE;
    goto busted;
  }

  /* attribute list has to be kept sorted per c14n rules */
  high = w->attributes.count; low = -1;
  while (high - low > 1)
  {
    int probe = (high + low) / 2;
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
  a->provided = False;
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
				   genxNamespace ns, constUtf8 name,
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
static genxStatus sendx(genxWriter w, constUtf8 s)
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

static genxStatus sendxBounded(genxWriter w, constUtf8 start, constUtf8 end)
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
static genxStatus writeStartTag(genxWriter w)
{
  int i;
  genxAttribute * aa = (genxAttribute *) w->attributes.pointers;
  genxElement e = w->nowStarting;

  /*
   * make sure the right namespace decls are in effect;
   *  if they are these might create an error, so ignore it
   */
  if (e->ns)
    addNamespace(e->ns, NULL);
  else
    unsetDefaultNamespace(w);
  w->status = GENX_SUCCESS;

  SendCheck(w, "<");
  if (e->ns && (e->ns->declaration != w->xmlnsEquals))
  {
    SendCheck(w, e->ns->declaration->name + STRLEN_XMLNS_COLON);
    SendCheck(w, ":");
  }
  SendCheck(w, e->type);

  for (i = 0; i < w->attributes.count; i++)
  {
    if (aa[i]->provided)
    {
      if (aa[i]->ns && aa[i]->ns->baroque &&
	  aa[i]->ns->declaration == w->xmlnsEquals)
	return w->status = GENX_ATTRIBUTE_IN_DEFAULT_NAMESPACE;

      SendCheck(w, " ");

      if (aa[i]->ns)
      {
	SendCheck(w, aa[i]->ns->declaration->name + STRLEN_XMLNS_COLON)
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

/*
 * internal clear-er; no sequence checking
 */
static genxStatus unsetDefaultNamespace(genxWriter w)
{
  int i;
  Boolean found = False;

  /* don't put it in if not needed */
  i = w->stack.count - 1;
  while (found == False && i > 0)
  {
    while (w->stack.pointers[i] != NULL)
    {
      genxAttribute decl = (genxAttribute) w->stack.pointers[i--];
      genxNamespace ns = (genxNamespace) w->stack.pointers[i--];

      /* if already unset */
      if (ns == NULL)
	return w->status = GENX_SUCCESS;

      /*
       * the default namespace was declared.  This namespace now
       *  becomes baroque
       */
      if (decl == w->xmlnsEquals)
      {
	ns->baroque = True;
	found = True;
	break;
      }
    }
    i -= 2;
  }

  if (!found)
    return GENX_SUCCESS;

  /*
   * push a signal on the stack
   */
  if ((w->status = listAppend(&w->stack, NULL)) != GENX_SUCCESS)
    return w->status;
  w->status = listAppend(&w->stack, w->xmlnsEquals);
  if (w->status != GENX_SUCCESS)
    return w->status;

  /* add the xmlns= attribute, it must be the first one */
  return addAttribute(w->xmlnsEquals, w->empty);
}

/*
 * clear the default namespace declaration
 */
genxStatus genxUnsetDefaultNamespace(genxWriter w)
{

  /* can only do this while in start-tag mode */
  if (w->sequence != SEQUENCE_START_TAG)
    return w->status = GENX_SEQUENCE_ERROR;

  return unsetDefaultNamespace(w);
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
  case SEQUENCE_ATTRIBUTES:
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
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
   * push the stack.  We push a NULL after a pointer to this element
   *  because the stack will also contain pointers to the namespace
   *  attributes that got declared here, so we can keep track of what's
   *  in effect.  I.e. a single stack entry consists logically of a pointer
   *  to an element object, a NULL, then zero or more pairs of pointers to
   *  namespace objects/declarations
   */
  if ((w->status = listAppend(&w->stack, e)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = listAppend(&w->stack, NULL)) != GENX_SUCCESS)
    return w->status;

  w->nowStarting = e;

  return GENX_SUCCESS;
}

/*
 * internal namespace adder; no sequence checking
 */
static genxStatus addNamespace(genxNamespace ns, utf8 prefix)
{
  genxWriter w = ns->writer;
  genxAttribute decl;
  int i;
  genxElement e;

  /*
   * first, we'll find the declaring attribute
   */
  if (prefix == NULL)
    decl = ns->defaultDecl;
  else
  {
    if (prefix[0] == 0)
      decl = w->xmlnsEquals;
    else
    {
      if ((prefix = storePrefix(w, prefix, True)) == NULL)
	return w->status;
      decl = declareAttribute(w, NULL, prefix, ns->name, &w->status);
      if (decl == NULL || w->status != GENX_SUCCESS)
	return w->status;
    }
  }

  if (decl != ns->defaultDecl)
    ns->baroque = True;

  /*
   * avoid doing anything if this namespace is already declared.  If
   *  they've shown good taste we can do this cheaply
   */
  if (!ns->baroque)
  {
    if (ns->declCount > 0)
      return w->status = GENX_SUCCESS;
  }
  else
  {

    /*
     * First, we'll run all the way up the stack to see if there is
     *  another declaration for this namespace/prefix in scope, in which
     *  case it's a no-op; or, if there's another declaration for this
     *  prefix on another namespace, in which case we have to over-ride
     */
    i = w->stack.count - 1;
    while (i > 0)
    {
      while (w->stack.pointers[i] != NULL)
      {
	genxAttribute otherDecl = (genxAttribute) w->stack.pointers[i--];
	genxNamespace otherNs = (genxNamespace) w->stack.pointers[i--];

	if (ns == otherNs)
	{
	  if (decl == otherDecl)
	    return w->status = GENX_SUCCESS;
	  else
	  {
	    i = 0;
	    break;
	  }
	}
	else
	{
	  /* different namespace, same prefix? */
	  if (decl == otherDecl)
	  {
	    i = 0;
	    break;
	  }
	}
      }
      i -= 2;
    }
  }

  /*
   * If this namespace is already declared on
   *  this element (with different prefix/decl) which is an error.
   */
  i = w->stack.count - 1;
  while (w->stack.pointers[i] != NULL)
  {
    genxNamespace otherNs;
    i--; /* don't need declaration */
    otherNs = (genxNamespace) w->stack.pointers[i--];

    if (ns == otherNs)
      return w->status = GENX_DUPLICATE_NAMESPACE;
  }

  /* move pointer from NULL to element */
  --i;

  /*
   * It's also an error if this is a default-namespace declaration and the
   *  element is in no namespace.
   */
  e = (genxElement) w->stack.pointers[i];    
  if (e->ns == NULL && decl == w->xmlnsEquals)
    return w->status = GENX_BAD_DEFAULT_DECLARATION;

  if ((w->status = listAppend(&w->stack, ns)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = listAppend(&w->stack, decl)) != GENX_SUCCESS)
    return w->status;

  ns->declaration = decl;
  ns->declCount++;
  return addAttribute(decl, ns->name);
}

/*
 * Add a namespace declaration
 */
genxStatus genxAddNamespace(genxNamespace ns, utf8 prefix)
{
  if (ns->writer->sequence != SEQUENCE_START_TAG)
    return ns->writer->status = GENX_SEQUENCE_ERROR;

  return addNamespace(ns, prefix);
}

/*
 * Private attribute-adding code
 * most of the work here is normalizing the value, which is the same
 *  as regular normalization except for " is replaced by "&quot;"
 */
static genxStatus addAttribute(genxAttribute a, constUtf8 valuestr)
{
  utf8 lastv = (utf8) valuestr;
  genxWriter w = a->writer;

  /* if valuestr not provided, this is an xmlns with a pre-cooked value */
  if (valuestr)
  {
    startCollect(&a->value);
    while (*valuestr)
    {
      int c = genxNextUnicodeChar(&valuestr);
      
      if (c == -1)
	return w->status = GENX_BAD_UTF8;
      
      if (!isXMLChar(w, c))
	return w->status = GENX_NON_XML_CHARACTER;
      
      switch(c)
      {
      case 9:
	collectPiece(w, &a->value, "&#x9;", 5);
	break;
      case 0xa:
	collectPiece(w, &a->value, "&#xA;", 5); 
	break;
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
	/*
      case '>':
	collectPiece(w, &a->value, "&gt;", 4);
	break;
	*/
      default:
	collectPiece(w, &a->value, (const char *) lastv, valuestr - lastv);
	break;
      }
      lastv = (utf8) valuestr;
    }
    endCollect(&a->value);
  }

  /* now add the namespace attribute; might fail if it's bee hand-declared */
  if (a->ns)
    addNamespace(a->ns, NULL);

  if (valuestr && a->provided)
    return w->status = GENX_DUPLICATE_ATTRIBUTE;
  a->provided = 1;

  return GENX_SUCCESS;
}

/*
 * public attribute adder.
 * The only difference is that it doesn't allow a NULL value
 */
genxStatus genxAddAttribute(genxAttribute a, constUtf8 valuestr)
{
  if (a->writer->sequence != SEQUENCE_START_TAG &&
      a->writer->sequence != SEQUENCE_ATTRIBUTES)
    return a->writer->status = GENX_SEQUENCE_ERROR;
  a->writer->sequence = SEQUENCE_ATTRIBUTES;

  if (valuestr == NULL)
    return a->writer->status = GENX_MISSING_VALUE;

  return addAttribute(a, valuestr);
}

genxStatus genxEndElement(genxWriter w)
{
  genxElement e;
  int i;

  switch (w->sequence)
  {
  case SEQUENCE_NO_DOC:
  case SEQUENCE_PRE_DOC:
  case SEQUENCE_POST_DOC:
    return w->status = GENX_SEQUENCE_ERROR;
  case SEQUENCE_START_TAG:
  case SEQUENCE_ATTRIBUTES:
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
      return w->status;
    break;
  case SEQUENCE_CONTENT:
    break;
  }

  /*
   * first peek into the stack to find the right namespace declaration
   *  (if any) so we can properly prefix the end-tag.  Have to do this
   *  before unwinding the stack because that might reset some xmlns
   *  prefixes to the context in the parent element
   */
  for (i = w->stack.count - 1; w->stack.pointers[i] != NULL; i -= 2)
    ;
  e = (genxElement) w->stack.pointers[--i];

  SendCheck(w, "</");
  if (e->ns && e->ns->declaration != w->xmlnsEquals)
  {
    SendCheck(w, e->ns->declaration->name + STRLEN_XMLNS_COLON);
    SendCheck(w, ":");
  }
  SendCheck(w, e->type);
  SendCheck(w, ">");

  /*
   * pop zero or more namespace declarations, then a null, then the
   *  start-element declaration off the stack
   */
  w->stack.count--;
  while (w->stack.pointers[w->stack.count] != NULL)
  {
    genxNamespace ns = (genxNamespace) w->stack.pointers[--w->stack.count];
    w->stack.count--; /* don't need decl */

    /* if not a fake unset-default namespace */
    if (ns)
    {
      /*
       * if they've stupidly jammed in their own namespace-prefix
       *  declarations, we have to go looking to see if there's another
       *  one in effect
       */
      if (ns->baroque)
      {
	i = w->stack.count;
	while (i > 0)
	{
	  while (w->stack.pointers[i] != NULL)
	  {
	    genxAttribute otherDecl = (genxAttribute) w->stack.pointers[i--];
	    genxNamespace otherNs = (genxNamespace) w->stack.pointers[i--];
	    
	    if (otherNs == ns)
	    {
	      ns->declaration = otherDecl;
	      i = 0;
	      break;
	    }
	  }
	  
	  /* skip NULL & element */
	  i -= 2;
	}
      }
      ns->declCount--;
      if (ns->declCount == 0)
	ns->baroque = False;
    }
  }

  /* pop the NULL */
  --w->stack.count;
  if (w->stack.count < 0)
    return w->status = GENX_NO_START_TAG;

  if (w->stack.count == 0)
    w->sequence = SEQUENCE_POST_DOC;
  else
    w->sequence = SEQUENCE_CONTENT;

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
static genxStatus addChar(genxWriter w, int c, constUtf8 next,
			  constUtf8 * lastsP, constUtf8 * breakerP)
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

genxStatus genxAddText(genxWriter w, constUtf8 start)
{
  constUtf8 lasts = start;
  constUtf8 breaker = start;

  if (w->sequence == SEQUENCE_START_TAG ||
      w->sequence == SEQUENCE_ATTRIBUTES)
  {
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  if (w->sequence != SEQUENCE_CONTENT)
    return w->status = GENX_SEQUENCE_ERROR;

  while (*start)
  {
    int c = genxNextUnicodeChar(&start);

    w->status = addChar(w, c, start, &lasts, &breaker);
    if (w->status != GENX_SUCCESS)
      return w->status;
  }
  return sendxBounded(w, breaker, (utf8) start);
}

genxStatus genxAddBoundedText(genxWriter w, constUtf8 start, constUtf8 end)
{
  constUtf8 lasts = start;
  constUtf8 breaker = start;

  if (w->sequence == SEQUENCE_START_TAG ||
      w->sequence == SEQUENCE_ATTRIBUTES)
  {
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  if (w->sequence != SEQUENCE_CONTENT)
    return w->status = GENX_SEQUENCE_ERROR;

  while (start < end)
  {
    int c = genxNextUnicodeChar(&start);

    w->status = addChar(w, c, (utf8) start, &lasts, &breaker);
    if (w->status != GENX_SUCCESS)
      return w->status;
  }
  return sendxBounded(w, breaker, (utf8) start);
}

genxStatus genxAddCountedText(genxWriter w, constUtf8 start, int byteCount)
{
  utf8 end = (utf8) (start + byteCount);

  return genxAddBoundedText(w, start, end);
}

genxStatus genxAddCharacter(genxWriter w, int c)
{
  unsigned char cUTF8[10];
  utf8 lasts, breaker, next;

  if (w->sequence == SEQUENCE_START_TAG ||
      w->sequence == SEQUENCE_ATTRIBUTES)
  {
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
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

  w->status =
    addChar(w, c, next, (constUtf8 *) &lasts, (constUtf8 *) &breaker);
  if (w->status != GENX_SUCCESS)
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

genxStatus genxComment(genxWriter w, constUtf8 text)
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

  if (w->sequence == SEQUENCE_START_TAG ||
      w->sequence == SEQUENCE_ATTRIBUTES)
  {
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
      return w->status;
    w->sequence = SEQUENCE_CONTENT;
  }

  else if (w->sequence == SEQUENCE_POST_DOC)
    if ((w->status = sendx(w, (utf8) "\n")) != GENX_SUCCESS)
      return w->status;

  if ((w->status = sendx(w, (utf8) "<!--")) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, (utf8) text)) != GENX_SUCCESS)
    return w->status;
  if ((w->status = sendx(w, (utf8) "-->")) != GENX_SUCCESS)
    return w->status;

  if (w->sequence == SEQUENCE_PRE_DOC)
    if ((w->status = sendx(w, (utf8) "\n")) != GENX_SUCCESS)
      return w->status;

  return GENX_SUCCESS;
}

genxStatus genxPI(genxWriter w, constUtf8 target, constUtf8 text)
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

  if (w->sequence == SEQUENCE_START_TAG ||
      w->sequence == SEQUENCE_ATTRIBUTES)
  {
    if ((w->status = writeStartTag(w)) != GENX_SUCCESS)
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
  if (text[0])
  {
    if ((w->status = sendx(w, (utf8) " ")) != GENX_SUCCESS)
      return w->status;
    if ((w->status = sendx(w, text)) != GENX_SUCCESS)
      return w->status;
  }
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
				   constUtf8 xmlns, constUtf8 type)
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

genxStatus genxAddAttributeLiteral(genxWriter w, constUtf8 xmlns,
				   constUtf8 name, constUtf8 value)
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
 
