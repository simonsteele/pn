#ifndef sscontainers_h__included
#define sscontainers_h__included

/**
 * @class Simple growing array...
 * @note This class is not safe for class types - constructors will not be called.
 */
template <typename T>
class GArray
{
	public:
		GArray() : iActualElements(0), iNumElements(0), pArray(NULL){}
		~GArray() { if(pArray) free(pArray); }
		
		void grow(int n)
		{
			if(n > iActualElements)
			{
				int n2 = n*2;

				if(pArray)
				{
					//re-allocate
					T* pNewArray = static_cast<T*>( realloc(pArray, n2 * sizeof(T)) );
					if(!pNewArray)
						throw "Could not re-allocate memory";
					pArray = pNewArray;
				}
				else
				{
					pArray = static_cast<T*>( malloc(n2 * sizeof(T)) );
				}
				iActualElements = n2;
			}
			iNumElements = n;
		}

		int size()
		{
			return iNumElements;
		}

		inline T& operator [] (int index)
		{
			PNASSERT(pArray != NULL);
			PNASSERT(index < iNumElements);
			return pArray[index];
		}

	protected:
		int iActualElements;
		int iNumElements;
		T*	pArray;
};

#endif //#ifndef sscontainers_h__included