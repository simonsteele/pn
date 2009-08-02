/**
 * @file boyermoore.h
 * @brief Boyer Moore Algorithm Implementation
 * @author Jan van den Baard
 * @note Copyright (c) 2000-2001 Jan van den Baard, All Rights Reserved.
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef _BOYERMOORE_H_
#define _BOYERMOORE_H_

// A class that implements the boyer-moore search,
// algorithm for forward and backward searches.
class BoyerMoore
{
public:
	// Construction, destruction.
	BoyerMoore();
	BoyerMoore( const BoyerMoore& bm );
	BoyerMoore( LPCSTR pszSearchString, BOOL bCaseOn = FALSE );
	virtual ~BoyerMoore();

	// Implementation.
	int FindForward( char *pData, int nLength );
	int FindBackward( char *pData, int nLength );
	
	void SetSearchString( LPCSTR pszSearchString );
	void SetCaseMode( BOOL bCase = TRUE );
	void SetMatchWholeWord( BOOL bMatchWholeWord = FALSE ); 
	void SetIncludeHidden( BOOL bIncludeHidden = TRUE );

	LPCSTR GetSearchString() const;
	bool GetCaseSensitive() const;

	// Operator overloads.
	const BoyerMoore& operator=( const BoyerMoore& bm );
	const BoyerMoore& operator=( LPCSTR pszSearchString );

protected:
	// Helpers.
	void SetSearchString();
	BOOL MatchWholeWord(char* pData, int nLength, int firstChar, int lastChar);

	// Data.
	std::string	m_strSearchString;
	BOOL	m_bCaseOn;
	BOOL	m_bMatchWholeWord;
	BOOL	m_bIncludeHidden;
	int		m_aDeltas[ 256 ];
	int		m_aBackDeltas[ 256 ];
};

#endif _BOYERMOORE_H_
