/**
 * @file boyermoore.cpp
 * @brief Boyer Moore Algorithm Implementation
 * @author Jan van den Baard
 * @note Copyright (c) 2000-2001 Jan van den Baard, All Rights Reserved.
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "boyermoore.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define _ASSERT_VALID(x) _ASSERT(x != NULL)

// Default constructor.
BoyerMoore::BoyerMoore()
{
	// Set default.
	m_bCaseOn = FALSE;
}

// Constructor. Set's search string and case mode.
BoyerMoore::BoyerMoore( LPCSTR pszSearchString, BOOL bCaseOn /* = FALSE  */ )
{
	_ASSERT_VALID( pszSearchString );

	// Setup data.
	m_strSearchString = pszSearchString;
	m_bCaseOn = bCaseOn;
	SetSearchString();
}

// Copy constructor.
BoyerMoore::BoyerMoore( const BoyerMoore& bm )
{
	// Copy data from source.
	operator=( bm );
}

// Destructor...
BoyerMoore::~BoyerMoore()
{
}

// Find forward.
#pragma warning( push )
#pragma warning( disable: 4127 )
int BoyerMoore::FindForward( char *pData, int nLength )
{
	_ASSERT_VALID( pData );
	_ASSERT( nLength > 0 );

	char		*pcEndString, *pcTextPtr, *pcString = ( char * )m_strSearchString.c_str();
	int		 nIndex, nSkip = 0, nStrLen = m_strSearchString.length() - 1;
	int		*lpTable = &m_aDeltas[ 0 ];

	// Setup data.
	pcEndString = ( char * )pcString + nStrLen;
	pcTextPtr   = pData + nStrLen;
	nIndex	    = nLength - nStrLen;

	// Enough text for a match?
	if ( nIndex >= 0 )
	{
		// Case-sensitive?
		if ( m_bCaseOn )
		{
			// Search...
			while ( 1 )
			{
				// Found string?
				if (( *pcEndString == *pcTextPtr ) && ( ! strncmp( pcString, pcTextPtr - nStrLen, nStrLen )))
				{
					// Yes. check first m_bMatchWholeWord, if ok -> Return the offset.
					int firstChar=( int )(( pcTextPtr - pData ) - nStrLen );
					if (m_bMatchWholeWord) {
						if (MatchWholeWord(pData, nLength, firstChar, firstChar+nStrLen)) {
							return firstChar;
						}
					}
					else {
						return firstChar;
					}
				}

				// Find out how many characters
				// we can skip.
				nSkip = lpTable[ ( UCHAR )tolower( *pcTextPtr ) ];

				// Enough left?
				if ( nIndex < nSkip )
					break;

				// Skip the characters.
				nIndex -= nSkip;
				pcTextPtr += nSkip;
			}
		}
		else
		{
			// Search...
			while ( 1 )
			{
				// Found string?
				if (( tolower( *pcEndString ) == tolower( *pcTextPtr )) && ( ! _strnicmp( pcString, pcTextPtr - nStrLen, nStrLen )))
				{
					// Yes. check first m_bMatchWholeWord, if ok -> Return the offset.
					int firstChar=( int )(( pcTextPtr - pData ) - nStrLen );
					if (m_bMatchWholeWord) {
						if (MatchWholeWord(pData, nLength, firstChar, firstChar+nStrLen)) {
							return firstChar;
						}
					}
					else {
						return firstChar;
					}
				}

				// Find out how many characters
				// we can skip.
				nSkip = lpTable[ ( UCHAR )tolower( *pcTextPtr ) ];

				// Enough left?
				if ( nIndex < nSkip )
					break;

				// Skip the characters.
				nIndex -= nSkip;
				pcTextPtr += nSkip;
			}
		}
	}
	return -1;
}

int BoyerMoore::FindBackward( char *pData, int nLength )
{
	_ASSERT_VALID( pData );
	_ASSERT( nLength > 0 );

	// Setup data.
	char			*pcString, *pcTextPtr;
	int			 nIndex, nSkip = 0, nStrLen = m_strSearchString.length() - 1;
	int			*lpTable = &m_aBackDeltas[ 0 ];

	pcString  = ( char * )m_strSearchString.c_str();
	pcTextPtr = pData;
	nIndex    = nLength;

	// Enough data to find the string in?
	if ( nIndex >= nStrLen )
	{
		// Case-sensitive?
		if ( m_bCaseOn )
		{
			// Search...
			while ( 1 )
			{
				// Found the string?
				if (( *pcString == *pcTextPtr ) && ( ! strncmp( pcString + 1, pcTextPtr + 1, nStrLen )))
				{
					// Yes. check first m_bMatchWholeWord, if ok -> Return the offset.
					int firstChar=( int )( pData - pcTextPtr );
					if (m_bMatchWholeWord) {
						if (MatchWholeWord(pData, nLength, firstChar, firstChar+nStrLen)) {
							return firstChar;
						}
					}
					else {
						return firstChar;
					}
				}
					// Return the index.
//					return ( int )( pData - pcTextPtr );

				// Find out how many characters
				// we can skip.
				nSkip = lpTable[ ( UCHAR )tolower( *pcTextPtr ) ];

				// Enough left?
				if ( nIndex < nSkip )
					break;

				// Skip the characters.
				nIndex -= nSkip;
				pcTextPtr -= nSkip;
			}
		}
		else
		{
			// Search...
			while ( 1 )
			{
				// Found the string?
				if (( tolower( *pcString ) == tolower( *pcTextPtr )) && ( ! _strnicmp( pcString + 1, pcTextPtr + 1, nStrLen )))
					// Return the index.
					return ( int )( pData - pcTextPtr );

				// Find out how many characters
				// we can skip.
				nSkip = lpTable[ ( UCHAR )tolower( *pcTextPtr ) ];

				// Enough left?
				if ( nIndex < nSkip )
					break;

				// Skip the characters.
				nIndex -= nSkip;
				pcTextPtr -= nSkip;
			}
		}
	}
	return -1;
}

BOOL BoyerMoore::MatchWholeWord(char* pData, int nLength, int firstChar, int lastChar) 
{
	if (firstChar) 
	{
		char ch = *(pData+firstChar-1);
		if (isalnum(ch) || ch == '_')
			return FALSE;
	}
	if (nLength > lastChar) 
	{
		char ch = *(pData+lastChar+1);
		if (isalnum(ch) || ch == '_')
			return FALSE;
	}
	return TRUE;
}


#pragma warning( pop )

// Operator overloads.
const BoyerMoore& BoyerMoore::operator=( const BoyerMoore& bm )
{
	_ASSERT( &bm != this );

	// Copy data from source.
	m_strSearchString = bm.m_strSearchString;
	m_bCaseOn = bm.m_bCaseOn;
	memcpy( m_aDeltas, bm.m_aDeltas, ( 0xFF + 1 ) * sizeof( int ));
	memcpy( m_aBackDeltas, bm.m_aBackDeltas, ( 0xFF + 1 ) * sizeof( int ));
	return *this;
}
const BoyerMoore& BoyerMoore::operator=( LPCSTR pszSearchString )
{
	_ASSERT_VALID( pszSearchString );

	// Setup data.
	m_strSearchString = pszSearchString;
	SetSearchString();
	return *this;
}

void BoyerMoore::SetSearchString()
{
	// Get search string length.
	int nLength = m_strSearchString.length();

	// Preset skip table values.
	int i;
	for ( i = 0; i <= 0xFF; i++ )
	{
		m_aDeltas[ i ] = nLength;
		m_aBackDeltas[ i ] = nLength;
	}

	// Setup skip values for
	// forward searches.
	LPCSTR pszPtr;
	for ( pszPtr = m_strSearchString.c_str(), i = nLength; --i > 0; )
	{
		m_aDeltas[ ( UCHAR )tolower( *pszPtr ) ] = i;
		pszPtr++;
	}

	// Setup skip values for
	// backward searches.
	for ( pszPtr = (( LPCSTR )m_strSearchString.c_str() ) + ( nLength - 1 ), i = nLength; --i > 0; )
	{
		m_aBackDeltas[ ( UCHAR )tolower( *pszPtr ) ] = i;
		pszPtr--;
	}
}

void BoyerMoore::SetSearchString( LPCSTR pszSearchString )
{ 
	_ASSERT_VALID( pszSearchString ); 
	m_strSearchString = pszSearchString; 
	SetSearchString();
}

void BoyerMoore::SetCaseMode( BOOL bCase ) 
{ 
	m_bCaseOn = bCase; 
	SetSearchString(); 
}

void BoyerMoore::SetMatchWholeWord( BOOL bMatchWholeWord ) 
{ 
	m_bMatchWholeWord = bMatchWholeWord; 
	SetSearchString(); 
}

void BoyerMoore::SetIncludeHidden( BOOL bIncludeHidden ) 
{ 
	m_bIncludeHidden = bIncludeHidden; 
	SetSearchString(); 
}

LPCSTR BoyerMoore::GetSearchString() const
{
	return m_strSearchString.c_str();
}

bool BoyerMoore::GetCaseSensitive() const
{
	return m_bCaseOn != 0;
}