/**
 * @file singleton.h
 * @brief Implement basic singletons - or at least a global access pattern.
 * @author Simon Steele
 * @note Copyright (c) 2002-2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef singleton_h__included
#define singleton_h__included

/**
 * Class to provide a base class with a virtual destructor that can be added
 * to a simple list of objects to delete. Used for singleton and orphan patterns.
 */
class DelObject
{
public:
	DelObject() : m_pNextToDelete(NULL){}
	virtual ~DelObject(){}
	DelObject* m_pNextToDelete;
};

/**
 * Class to provide managed singleton/orphan deletion.
 */
class DeletionManager
{
public:
	static void Register(DelObject* pObject);
	static void UnRegister(DelObject* pObject);
	static void DeleteAll();
	static DelObject* s_pFirst;
	static DelObject* s_pLast;
};

/**
 * Class to provide simple singleton functionality.
 */
template <class T, bool t_bAutoRegister = false>
class Singleton : public DelObject
{
	public:
		virtual ~Singleton(){}

		static bool HasInstance()
		{
			return s_pTheInstance != NULL;
		}

		static T* GetInstance()
		{
			if(!s_pTheInstance)
			{
				s_pTheInstance = T::CreateTheInstance();
			}

			return s_pTheInstance;
		}

		/// Subclasses can override this to create their single instance...
		static T* CreateTheInstance()
		{
			T* pT = new T;
			if(t_bAutoRegister)
			{
				DeletionManager::Register(pT);
			}
			return pT;
		}

		static T& GetInstanceRef()
		{
			return *GetInstance();
		}

		static void ReleaseInstance()
		{
			if(s_pTheInstance)
			{
				delete s_pTheInstance;
				s_pTheInstance = NULL;
			}
		}

	protected:
		static T* s_pTheInstance;
};

template <class T, bool t_bAutoRegister>
T* Singleton< T, t_bAutoRegister >::s_pTheInstance = NULL;

#define SINGLETON_AUTO_DELETE true

#endif // #ifndef singleton_h__included