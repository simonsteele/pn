/**
 * @file string.h
 * @brief Simple string class safe across DLL boundaries
 * @author Simon Steele
 * @note Copyright (c) 2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef string_h__included
#define string_h__included

namespace PN {

/**
 * Yes boys and girls, it's another string class! We have to 
 * do this for DLL boundary strings - the extensions interface
 */
class BaseString
{
public:
	virtual void Add(const char * str, size_t length = ~0) = 0;
	virtual void Set(const char * str, size_t length = ~0) {Reset(); Add(str, length);}
	virtual void Truncate(size_t length) = 0;

	virtual size_t GetLength() const { return strlen(Get()); }
	virtual const char * Get() const = 0;
	
	virtual char * LockBuffer(size_t requested) = 0;
	virtual void UnlockBuffer() = 0;
	
	inline void Reset() { Truncate(0); }
	
	inline bool Empty() const { return *Get() == 0; }

	inline const BaseString & operator = (const char * src) { Set(src); return *this; }
	inline const BaseString & operator += (const char * src) { Add(src); return *this; }
	inline const BaseString & operator = (const BaseString & src) { Set(src); return *this; }
	inline const BaseString & operator += (const BaseString & src) { Add(src); return *this; }
	inline const BaseString & operator += (const char src) { Add(&src, 1); return *this; }
	inline bool operator == (const char* other) const { return strcmp(Get(), other) == 0; }
	inline bool operator == (const BaseString& other) const { return strcmp(Get(), other.Get()) == 0; }
	inline bool operator != (const char* other) const { return !(*this == other); }
	inline bool operator != (const BaseString& other) const { return !(*this == other); }

	inline operator const char * () const { return Get(); }

protected:
	BaseString() {}
	~BaseString() {}
};

template <class TAlloc>
class AsciiString : public BaseString
{
	typedef AsciiString<TAlloc> TThis;
public:
	AsciiString() : m_buffer(0), m_size(0), m_used(0)
	{}

	AsciiString(const char* str) : m_buffer(0), m_size(0), m_used(0)
	{
		Set(str);
	}

	~AsciiString()
	{
		if (m_buffer != NULL)
		{
			m_allocator.Free(m_buffer);
		}
	}

	virtual void Add(const char * str, size_t length = ~0)
	{
		PNASSERT(str != NULL);

		if(length == ~0)
		{
			length = strlen(str);
		}

		add(str, length);
	}

	virtual void Truncate(size_t length)
	{
		if(length < m_used)
		{
			m_buffer[length] = '\0';
			m_used = length;
		}
	}

	virtual const char* Get() const
	{
		if(m_used)
		{
			return m_buffer;
		}
		else
		{
			return "";
		}
	}

	virtual char* LockBuffer(size_t requested)
	{
		allocate(requested);
		return m_buffer;
	}

	virtual void UnlockBuffer()
	{
		m_used = strlen(m_buffer);
	}

	// Re-implemented for cross-type operators:
	inline const TThis & operator = (const char * src) { Set(src); return *this; }
	inline const TThis & operator += (const char * src) { Add(src); return *this; }
	inline const TThis & operator = (const BaseString & src) { Set(src); return *this; }
	inline const TThis & operator += (const BaseString & src) { Add(src); return *this; }
	inline const TThis & operator += (const char src) { Add(&src, 1); return *this; }
	inline const TThis & operator = (const TThis & src) { Set(src); return *this; }
	inline const TThis & operator += (const TThis & src) { Add(src); return *this; }
	
private:
	void add(const char* str, size_t length)
	{
		allocate(m_used + length);
		memcpy(&m_buffer[m_used], str, length);
		m_used += length;
		m_buffer[m_used] = '\0';
	}

	void allocate(size_t length)
	{
		if(m_size)
		{
			if(length >= m_size)
			{
				char* newBuffer = m_allocator.ReAlloc(m_buffer, m_size, length + 16);
				if(newBuffer != NULL)
				{
					m_buffer = newBuffer;
					m_size = length + 16;
				}
				else
				{
					throw std::exception("out of memory in AsciiString.allocate");
				}
			}
		}
		else
		{
			m_buffer = m_allocator.Alloc(length + 16);
			if(m_buffer != NULL)
			{
				m_size = length + 16;
			}
		}
	}

	char* m_buffer;
	size_t m_size;
	size_t m_used;
	TAlloc m_allocator;
};

typedef AsciiString< BasicAllocator<char> > AString;

const AString EmptyString("");

} // namespace PN

#endif