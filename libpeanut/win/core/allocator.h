#ifndef PN_WIN_CORE_ALLOCATOR_
#define PN_WIN_CORE_ALLOCATOR_

template <typename T>
class LocalAllocAllocator : public WastefulReallocAllocator<T>
{
public:
	LocalAllocAllocator()
	{
		PNASSERT(Traits<T>::RequiresConstruction == false);
	}

	virtual T* Alloc(size_t noofTs)
	{
		T* ptrs = static_cast<T*>( ::LocalAlloc(LMEM_FIXED, noofTs * sizeof(T)) );
		// if we want to support constructors we must do that here...
		return ptrs;
	}

	virtual void Free(T* mem)
	{
		::LocalFree(mem);
	}
};


#endif PN_WIN_CORE_ALLOCATOR_
