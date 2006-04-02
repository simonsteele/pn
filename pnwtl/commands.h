#ifndef commands_h__included
#define commands_h__included

#define K_ALT		FALT
#define K_CTRL		FCONTROL
#define K_SHIFT		FSHIFT
#define K_CTRLALT	(K_ALT | K_CTRL)
#define K_CTRLSHIFT	(K_SHIFT | K_CTRL)
#define K_ALTSHIFT	(K_SHIFT | K_ALT)

/**
 */
struct KeyToCommand {
	unsigned char modifiers;
	unsigned char key;
	unsigned int msg;
};

/**
 */
class KeyMap {
public:
	KeyMap();
	~KeyMap();
	void Clear();
	void AssignCmdKey(int key, int modifiers, unsigned int msg);
	unsigned int Find(int key, int modifiers);	// 0 returned on failure
private:
	KeyToCommand *kmap;
	int len;
	int alloc;
	static const KeyToCommand MapDefault[];
};

#endif // #ifndef commands_h__included