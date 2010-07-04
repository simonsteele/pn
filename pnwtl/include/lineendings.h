bool isCR_BE(unsigned char* buf)
{
	return buf[0] == 0 && buf[1] == '\r';
}

bool isCR_LE(unsigned char* buf)
{
	return buf[0] == '\r' && buf[1] == 0;
}

bool isLF_BE(unsigned char* buf)
{
	return buf[0] == 0 && buf[1] == '\n';
}

bool isLF_LE(unsigned char* buf)
{
	return buf[0] == '\n' && buf[1] == 0;
}

typedef bool(*char_is_fn)(unsigned char*);

EPNSaveFormat determineLineEndings(unsigned char* pBuf, int nLen, EPNEncoding encoding)
{
	int linesCRLF, linesCR, linesLF;
	
	linesCRLF = linesCR = linesLF = 0;

	if(encoding == eUtf16BigEndian || encoding == eUtf16LittleEndian)
	{
		// 16-bit
		char_is_fn isLF;
		char_is_fn isCR;
		
		if(encoding == eUtf16BigEndian)
		{
			isLF = isLF_BE;
			isCR = isCR_BE;
		}
		else
		{
			isLF = isLF_LE;
			isCR = isCR_LE;
		}
		
		unsigned char *c, *n;
		for(int i = 0; i < nLen; i += 2)
		{
			c = &pBuf[i];
			n = ((i < nLen-1) ? &pBuf[i+2] : NULL);

			if (isCR(c))
			{
				if (n && isLF(n))
				{
					linesCRLF++;
					// Skip the next character (\n).
					i++;
					continue;
				}
				else
					linesCR++;
			} 
			else if (isLF(c)) 
			{
				linesLF++;
			}
		}
	}
	else
	{
		char c, n;

		// 8-bit...
		for(int i = 0; i < nLen; i++)
		{
			c = pBuf[i];
			n = ((i < nLen) ? pBuf[i+1] : NULL);

			if (c == '\r') 
			{
				if (n == '\n')
				{
					linesCRLF++;
					// Skip the next character (\n).
					i++;
					continue;
				}
				else
					linesCR++;
			} 
			else if (c == '\n') 
			{
				linesLF++;
			}
		}
	}

	if (((linesLF >= linesCR) && (linesLF > linesCRLF)) || ((linesLF > linesCR) && (linesLF >= linesCRLF)))
		return PNSF_Unix;
	else if (((linesCR >= linesLF) && (linesCR > linesCRLF)) || ((linesCR > linesLF) && (linesCR >= linesCRLF)))
		return PNSF_Mac;
	
	else if (((linesCRLF >= linesLF) && (linesCRLF > linesCR)) || ((linesCRLF > linesLF) && (linesCRLF >= linesCR)))
		return PNSF_Windows;

	// Default
	return (EPNSaveFormat)OPTIONS->GetCached(Options::OLineEndings);
}
