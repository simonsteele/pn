/**
 * @file callback.h
 * @brief Define callback template classes.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes
 * the conditions under which this source may be modified / distributed.
 *
 * Implementations of CallbackBase and Callback copied almost entirely
 * from an article (GotW #83) by Herb Sutter.
 */

#ifndef callback_h__included
#define callback_h__included

class CallbackBase
{
	public:
		virtual void operator() () const { };
		virtual ~CallbackBase(){}
};

template<typename T>
class Callback : public CallbackBase
{
	public:
		typedef void (T::*F)();

		Callback( T& t, F f ) : m_t(&t), m_f(f) { }
		void operator() () const { (m_t->*m_f) (); }

	private:
		T*	m_t;
		F	m_f;
};

template<typename ret, typename param>
class CallbackBase2
{
	public:
		virtual ret operator() (param ptr) const = 0;
		virtual ~CallbackBase2(){} // not sure why this must be implemented...
};

template<typename T, typename C, typename ret>
class CallbackClassPtr : public CallbackBase2<ret, C>
{
	public:
		typedef ret (T::*F)(C param);

		CallbackClassPtr( T& t, F f ) : m_t(&t), m_f(f) { }
		ret operator() (C param) const { return (m_t->*m_f) (param); }

	private:
		T*	m_t;
		F	m_f;
};

template<typename T>
Callback<T> make_callback( T& t, void (T::*f) () )
{
	return Callback<T>( t, f );
}

#endif //callback_h__included