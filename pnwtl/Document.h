/**
 * @file Document.h
 * @brief PN Document
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47
#define document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47

class CChildFrame;

class Document
{
	friend class CChildFrame;

	public:
		Document(LPCTSTR filename = NULL);

		void AddChildFrame(CChildFrame* pFrame);

		bool IsValid() const;

		bool FileExists() const;

		long GetFileAge() const;
		tstring GetFileName(EGFNType type = FN_FULL) const;
		CChildFrame* GetFrame() const;
		bool HasFile() const;

		void SetFileName(LPCTSTR filename);

// Protected members for friend classes...
	protected:
		void SetValid(bool bValid);

	protected:
		CChildFrame*	m_pFrame;
		bool			m_bIsValid;
		tstring			m_sFilename;
};

#endif // #ifndef document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47