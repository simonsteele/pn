#ifndef projectwriter_h__included
#define projectwriter_h__included

namespace Projects
{

typedef struct tagProjectWriter
{
	genxElement eFile;
	genxAttribute aFilePath;
	genxWriter w;
} SProjectWriter, * ProjectWriter;

#define u(x) (utf8)x

}

#endif //#ifndef projectwriter_h__included