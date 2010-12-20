// Copyright (c) 2006
// Sergey Klimov (kidd@ukr.net)

#ifndef __WTL_DW__STG_H__
#define __WTL_DW__STG_H__

#include <vector>
#include <sstream>
#include <iomanip>
#include <iterator>
#include <algorithm>

namespace sstate{

class IStorge
{
public:
	enum Modes{Read,Write,ReadWrite};
public:
	virtual ~IStorge(void){}
	virtual long Open(IStorge& /*parent*/,LPCTSTR /*name*/,Modes /*mode*/)=0;
	virtual long Create(IStorge& /*parent*/,LPCTSTR /*name*/,Modes /*mode*/)=0;
	virtual long SetString(LPCTSTR /*name*/,LPCTSTR /*data*/)=0;
	virtual long GetString(LPCTSTR /*name*/,LPTSTR /*data*/,size_t& /*size*/)=0;
//	virtual long SetBinary(LPCTSTR /*name*/,const void* /*data*/,size_t /*size*/)=0;
//	virtual long GetBinary(LPCTSTR /*name*/,void* /*data*/,size_t& /*size*/)=0;
	virtual long SetBinary(LPCTSTR name,const void* data,size_t size)
	{
		std::basic_ostringstream<TCHAR> out;
		out.setf(std::ios::hex,std::ios::basefield);
		for(size_t i=0; i<size; ++i)
			out<<std::setw(2)<<std::setfill(_T('0'))<<std::char_traits<char>::to_int_type(*(reinterpret_cast<const char*>(data)+i))<<_T(' ');
		return SetString(name,out.str().c_str());
	}

 	virtual long GetBinary(LPCTSTR name,void* data,size_t& size)
 	{
		size_t len=0;
		long res=GetString(name,static_cast<TCHAR*>(data),len);
		if(res==ERROR_MORE_DATA)
		{
			std::vector<TCHAR> buf(++len);
			res=GetString(name,&buf[0],len);
			if(res==ERROR_SUCCESS)
			{
				std::vector<char> tmp;
				tmp.reserve(len/3); //estimated size
				std::basic_istringstream<TCHAR> in(&buf[0]);
				in.setf(std::ios::hex,std::ios::basefield);
				std::copy(std::istream_iterator<int,TCHAR>(in),std::istream_iterator<int,TCHAR>(),std::back_inserter(tmp));
				len=tmp.size();
				if(size>=len)
					std::copy(tmp.begin(),tmp.end(),static_cast<char*>(data));
				else
					res=ERROR_MORE_DATA;
			}
		}
		size=len;
		return res;
 	}
};

}//namespace sstate

#endif // __WTL_DW__STG_H__