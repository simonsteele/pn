/**
 * @file sslistctrl.h
 * @brief A WTL list control subclass with some useful functionality.
 * @author Simon Steele
 * @note Copyright (c) 2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef shellicons_h__included
#define shellicons_h__included

class IconExtractor
{
	public:
		HICON IconForFile(LPCTSTR filename)
		{
			SHFILEINFO info;
			if( ::SHGetFileInfo(filename, 0, &info, sizeof(SHFILEINFO), 
				SHGFI_ICON | SHGFI_SMALLICON) != 0 )
			{
				return info.hIcon;
			}
			else
				return NULL;
		}

		HICON DefaultIcon()
		{
			SHFILEINFO info;
			if( ::SHGetFileInfo(_T(".stupidextension"), FILE_ATTRIBUTE_NORMAL, &info, sizeof(SHFILEINFO),
				SHGFI_ICON | SHGFI_SMALLICON | SHGFI_USEFILEATTRIBUTES ) != 0 )
			{
				return info.hIcon;
			}
			else
				return NULL;
		}

		HICON FolderIcon()
		{
			SHFILEINFO info;
			if( ::SHGetFileInfo(_T("somefolder"), FILE_ATTRIBUTE_DIRECTORY, &info, sizeof(SHFILEINFO),
				SHGFI_ICON | SHGFI_SMALLICON | SHGFI_USEFILEATTRIBUTES ) != 0 )
			{
				return info.hIcon;
			}
			else
				return NULL;
		}
};

class ShellImageList
{
	public:
		ShellImageList()
		{
			/* Use a 32-bit ImageList for Common Controls 6 and greater to
			get a nice alpha channel folder image, but on earlier versions
			this will lead to a nasty dithering effect, so drop back to 8-bit. */
			const UINT flags[2] = {ILC_COLOR8 | ILC_MASK, ILC_COLOR32 | ILC_MASK};

			images = ::ImageList_Create(16, 16, flags[GetComCtlVersion() >= 0x60000], 2, 4);
			
			// Folder Icon
			::ImageList_AddIcon(images, extractor.FolderIcon());

			// Default unknown extension icon.
			::ImageList_AddIcon(images, extractor.DefaultIcon());
		}

		~ShellImageList()
		{
			::ImageList_Destroy(images);
		}

		/// Add a user icon to the collection. Returns the icon index.
		int AddIcon(HICON hIcon)
		{
			return ::ImageList_AddIcon(images, hIcon);
		}

		/// Returns an index into the image list this class wraps for the
		/// given filename.
		int IndexForFile(LPCTSTR filename)
		{
			CFileName fn(filename);
			tstring ext = fn.GetExtension();

			if(_tcsicmp(ext.c_str(), _T(".ico")) != 0)
			{
				std::map<tstring, int>::const_iterator i = extIndexes.find(ext);

				if(i != extIndexes.end())
				{
					return (*i).second;
				}
			}

			HICON icon = extractor.IconForFile(filename);
			if(icon != NULL)
			{
				int index = ::ImageList_AddIcon(images, icon);
				extIndexes.insert(std::map<tstring, int>::value_type(ext, index));
				::DestroyIcon(icon);
				return index;
			}
			else
				return 1;
		}

		HIMAGELIST GetImageList()
		{
			return images;
		}

	protected:
		static int GetComCtlVersion()
		{
			static int ComCtlVersion = 0;

			const TCHAR* FileName = _T("comctl32.dll");
			DWORD InfoSize, Dummy = 0;
			BYTE* buf;
			UINT VerSize;
			VS_FIXEDFILEINFO * ffi;

			if(ComCtlVersion == 0)
			{
				// Dummy gets set to 0.
				InfoSize = ::GetFileVersionInfoSize(FileName, &Dummy);
				if(InfoSize > 0)
				{
					buf = new BYTE[InfoSize];
					if (::GetFileVersionInfo(FileName, Dummy, InfoSize, buf))
					{
						if(::VerQueryValue(buf, _T("\\"), (void**)&ffi, &VerSize))
						{
							ComCtlVersion = ffi->dwFileVersionMS;
						}
					}

					delete [] buf;
				}
			}

			return ComCtlVersion;
		}

	protected:
		std::map<tstring, int>	extIndexes;
		HIMAGELIST				images;
		IconExtractor			extractor;
};

#endif