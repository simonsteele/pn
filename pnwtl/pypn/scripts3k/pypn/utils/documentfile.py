import pn, scintilla

class DocumentFile(object):
    """file-like interface to new Scintilla document editor
    DocumentFile() -> file object

    Open a file on a new Scintilla document editor.
    Provide the ability to add text to a new document
    editor using the python file idiom.
    
    Also allows you to treat an existing document as
    a file by passing document to constructor."""
    
    def __init__(self, pndoc = None):
        """x.__init__(...) initializes x; see x.__class__.__doc__ for signature"""
        self._closed = False
        self._softspace = 0
        if not pndoc == None:
            self.doc = pndoc
            self.__editor = scintilla.Scintilla(self.doc)
        else:
            doc = pn.NewDocument(None)
            self.__editor = scintilla.Scintilla(pn.CurrentDoc())
            self.__editor.BeginUndoAction()

    def close(self):
        """close() -> None or (perhaps) an integer.  Close the file.

        Sets data attribute .closed to True.  A closed file cannot be used for
        further I/O operations.  close() may be called more than once without
        error.  Some kinds of file objects (for example, opened by popen())
        may return an exit status upon closing."""
        if not self._closed:
            self.__editor.EndUndoAction()
        self._closed = True
        self.__editor = None

    def flush(self):
        """flush() -> None.  Flush the internal I/O buffer. In this case, a no-op"""
        pass

    def write(self,s):
        """write(str) -> None.  Write string str to file.

        Note that due to buffering, flush() or close() may be needed before
        the file on disk reflects the data written."""
        if self._closed:
            return
        self.__editor.AppendText(len(s), s)

    def writelines(self,ss):
        """writelines(sequence_of_strings) -> None.  Write the strings to the file.

        Note that newlines are not added.  The sequence can be any iterable object
        producing strings. This is equivalent to calling write() for each string."""
        for s in ss:
            self.write(s)

    def _get_closed(self): return self._closed
    def _set_closed(self,_closed): self._closed = _closed
    closed = property(_get_closed,_set_closed,None,
        "True if the file is closed")

    def _get_softspace(self): return self._softspace
    def _set_softspace(self,_softspace): self._softspace = _softspace
    softspace = property(_get_softspace,_set_softspace,None,
        "flag indicating that a space needs to be printed; used by print")