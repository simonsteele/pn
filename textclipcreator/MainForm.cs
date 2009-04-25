using System;
using System.IO;
using System.Windows.Forms;

namespace TextClipCreator
{
	/// <summary>
	/// Main TextClipCreator Form
	/// </summary>
	public class MainForm : System.Windows.Forms.Form
	{
		const string UserDataDirectory = @"Echo Software\PN2";

		private System.Windows.Forms.OpenFileDialog openFileDialog;
		private System.Windows.Forms.ListBox lstClips;
		private System.Windows.Forms.TextBox txtClipText;
		private System.Windows.Forms.TextBox txtClipName;
		private System.Windows.Forms.Button btnUpdate;
		private System.Windows.Forms.Button btnDiscard;
		/// <summary>Required designer variable.</summary>
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.ErrorProvider errorProvider;
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem itmNew;
		private System.Windows.Forms.MenuItem itmOpen;
		private System.Windows.Forms.MenuItem itmSave;
		private System.Windows.Forms.MenuItem itmSaveAs;
		private System.Windows.Forms.MenuItem menuItem1;
		private System.Windows.Forms.MenuItem menuItem3;
		private System.Windows.Forms.MenuItem menuItem5;
		private System.Windows.Forms.MenuItem itmExit;
		private System.Windows.Forms.Button btnAdd;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.TextBox setName;
		private System.Windows.Forms.GroupBox groupBox1;
		private System.Windows.Forms.SaveFileDialog saveFileDialog;
		private System.Windows.Forms.Label lblClipName;
		private System.Windows.Forms.Button btnRemove;
		
		bool _changing = false;
		Clips _clips = null;
		ClipStorage _cs = new ClipStorage();
		string _filename = null;
		bool _editing = false;

		public MainForm()
		{
			InitializeComponent();

			enableButtons(false);

			// REVIEW: check for write access to ProgramFiles?
			openFileDialog.InitialDirectory = GetUserDataPath();
			saveFileDialog.InitialDirectory = GetUserDataPath();
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
			this.btnAdd = new System.Windows.Forms.Button();
			this.lstClips = new System.Windows.Forms.ListBox();
			this.txtClipText = new System.Windows.Forms.TextBox();
			this.btnUpdate = new System.Windows.Forms.Button();
			this.btnDiscard = new System.Windows.Forms.Button();
			this.lblClipName = new System.Windows.Forms.Label();
			this.txtClipName = new System.Windows.Forms.TextBox();
			this.errorProvider = new System.Windows.Forms.ErrorProvider();
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.menuItem1 = new System.Windows.Forms.MenuItem();
			this.itmNew = new System.Windows.Forms.MenuItem();
			this.menuItem5 = new System.Windows.Forms.MenuItem();
			this.itmOpen = new System.Windows.Forms.MenuItem();
			this.itmSave = new System.Windows.Forms.MenuItem();
			this.itmSaveAs = new System.Windows.Forms.MenuItem();
			this.menuItem3 = new System.Windows.Forms.MenuItem();
			this.itmExit = new System.Windows.Forms.MenuItem();
			this.label2 = new System.Windows.Forms.Label();
			this.setName = new System.Windows.Forms.TextBox();
			this.groupBox1 = new System.Windows.Forms.GroupBox();
			this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
			this.btnRemove = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// openFileDialog
			// 
			this.openFileDialog.Filter = "Clips Files|*.clips|All Files|*.*";
			// 
			// btnAdd
			// 
			this.btnAdd.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
			this.btnAdd.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnAdd.Location = new System.Drawing.Point(8, 288);
			this.btnAdd.Name = "btnAdd";
			this.btnAdd.Size = new System.Drawing.Size(72, 24);
			this.btnAdd.TabIndex = 4;
			this.btnAdd.Text = "&Add";
			this.btnAdd.Click += new System.EventHandler(this.btnAdd_Click);
			// 
			// lstClips
			// 
			this.lstClips.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
				| System.Windows.Forms.AnchorStyles.Left)));
			this.lstClips.IntegralHeight = false;
			this.lstClips.Location = new System.Drawing.Point(8, 40);
			this.lstClips.Name = "lstClips";
			this.lstClips.Size = new System.Drawing.Size(152, 240);
			this.lstClips.TabIndex = 3;
			this.lstClips.SelectedValueChanged += new System.EventHandler(this.lstClips_SelectedValueChanged);
			// 
			// txtClipText
			// 
			this.txtClipText.AcceptsReturn = true;
			this.txtClipText.AcceptsTab = true;
			this.txtClipText.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
				| System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right)));
			this.txtClipText.Font = new System.Drawing.Font("Courier New", 8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.txtClipText.Location = new System.Drawing.Point(176, 69);
			this.txtClipText.Multiline = true;
			this.txtClipText.Name = "txtClipText";
			this.txtClipText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
			this.txtClipText.Size = new System.Drawing.Size(424, 211);
			this.txtClipText.TabIndex = 8;
			this.txtClipText.Text = "";
			this.txtClipText.WordWrap = false;
			this.txtClipText.TextChanged += new System.EventHandler(this.txtClipName_TextChanged);
			// 
			// btnUpdate
			// 
			this.btnUpdate.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnUpdate.Enabled = false;
			this.btnUpdate.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnUpdate.Location = new System.Drawing.Point(448, 289);
			this.btnUpdate.Name = "btnUpdate";
			this.btnUpdate.Size = new System.Drawing.Size(72, 24);
			this.btnUpdate.TabIndex = 9;
			this.btnUpdate.Text = "&Update";
			this.btnUpdate.Click += new System.EventHandler(this.btnUpdate_Click);
			// 
			// btnDiscard
			// 
			this.btnDiscard.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnDiscard.Enabled = false;
			this.btnDiscard.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnDiscard.Location = new System.Drawing.Point(527, 289);
			this.btnDiscard.Name = "btnDiscard";
			this.btnDiscard.Size = new System.Drawing.Size(72, 24);
			this.btnDiscard.TabIndex = 10;
			this.btnDiscard.Text = "&Discard";
			this.btnDiscard.Click += new System.EventHandler(this.btnDiscard_Click);
			// 
			// lblClipName
			// 
			this.lblClipName.Location = new System.Drawing.Point(176, 43);
			this.lblClipName.Name = "lblClipName";
			this.lblClipName.Size = new System.Drawing.Size(64, 16);
			this.lblClipName.TabIndex = 6;
			this.lblClipName.Text = "&Clip Name:";
			// 
			// txtClipName
			// 
			this.txtClipName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right)));
			this.txtClipName.Location = new System.Drawing.Point(248, 40);
			this.txtClipName.Name = "txtClipName";
			this.txtClipName.Size = new System.Drawing.Size(352, 20);
			this.txtClipName.TabIndex = 7;
			this.txtClipName.Text = "";
			this.txtClipName.TextChanged += new System.EventHandler(this.txtClipName_TextChanged);
			// 
			// errorProvider
			// 
			this.errorProvider.ContainerControl = this;
			// 
			// mainMenu1
			// 
			this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.menuItem1});
			// 
			// menuItem1
			// 
			this.menuItem1.Index = 0;
			this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.itmNew,
																					  this.menuItem5,
																					  this.itmOpen,
																					  this.itmSave,
																					  this.itmSaveAs,
																					  this.menuItem3,
																					  this.itmExit});
			this.menuItem1.Text = "&File";
			// 
			// itmNew
			// 
			this.itmNew.Index = 0;
			this.itmNew.Text = "&New";
			this.itmNew.Click += new System.EventHandler(this.itmNew_Click);
			// 
			// menuItem5
			// 
			this.menuItem5.Index = 1;
			this.menuItem5.Text = "-";
			// 
			// itmOpen
			// 
			this.itmOpen.Index = 2;
			this.itmOpen.Shortcut = System.Windows.Forms.Shortcut.CtrlO;
			this.itmOpen.Text = "&Open";
			this.itmOpen.Click += new System.EventHandler(this.itmOpen_Click);
			// 
			// itmSave
			// 
			this.itmSave.Enabled = false;
			this.itmSave.Index = 3;
			this.itmSave.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
			this.itmSave.Text = "&Save";
			this.itmSave.Click += new System.EventHandler(this.itmSave_Click);
			// 
			// itmSaveAs
			// 
			this.itmSaveAs.Index = 4;
			this.itmSaveAs.Text = "Save &As...";
			this.itmSaveAs.Click += new System.EventHandler(this.itmSaveAs_Click);
			// 
			// menuItem3
			// 
			this.menuItem3.Index = 5;
			this.menuItem3.Text = "-";
			// 
			// itmExit
			// 
			this.itmExit.Index = 6;
			this.itmExit.Text = "E&xit";
			this.itmExit.Click += new System.EventHandler(this.itmExit_Click);
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(8, 10);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(64, 16);
			this.label2.TabIndex = 1;
			this.label2.Text = "&Set Name:";
			// 
			// setName
			// 
			this.setName.Location = new System.Drawing.Point(72, 7);
			this.setName.Name = "setName";
			this.setName.Size = new System.Drawing.Size(160, 20);
			this.setName.TabIndex = 2;
			this.setName.Text = "";
			this.setName.TextChanged += new System.EventHandler(this.setName_TextChanged);
			// 
			// groupBox1
			// 
			this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right)));
			this.groupBox1.Location = new System.Drawing.Point(8, 32);
			this.groupBox1.Name = "groupBox1";
			this.groupBox1.Size = new System.Drawing.Size(592, 2);
			this.groupBox1.TabIndex = 9;
			this.groupBox1.TabStop = false;
			// 
			// saveFileDialog
			// 
			this.saveFileDialog.Filter = "Text Clip Files|*.clips|All Files|*.*";
			// 
			// btnRemove
			// 
			this.btnRemove.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
			this.btnRemove.Enabled = false;
			this.btnRemove.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnRemove.Location = new System.Drawing.Point(88, 288);
			this.btnRemove.Name = "btnRemove";
			this.btnRemove.Size = new System.Drawing.Size(72, 24);
			this.btnRemove.TabIndex = 5;
			this.btnRemove.Text = "&Remove";
			this.btnRemove.Click += new System.EventHandler(this.btnRemove_Click);
			// 
			// MainForm
			// 
			this.AcceptButton = this.btnUpdate;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(608, 321);
			this.Controls.Add(this.btnRemove);
			this.Controls.Add(this.groupBox1);
			this.Controls.Add(this.setName);
			this.Controls.Add(this.txtClipName);
			this.Controls.Add(this.txtClipText);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.lblClipName);
			this.Controls.Add(this.btnDiscard);
			this.Controls.Add(this.btnUpdate);
			this.Controls.Add(this.lstClips);
			this.Controls.Add(this.btnAdd);
			this.Menu = this.mainMenu1;
			this.Name = "MainForm";
			this.Text = "PN2 Text Clip Creator";
			this.Closing += new System.ComponentModel.CancelEventHandler(this.MainForm_Closing);
			this.ResumeLayout(false);

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.EnableVisualStyles();
			Application.DoEvents();
			Application.Run(new MainForm());
		}

		private void MainForm_Closing(object sender, System.ComponentModel.CancelEventArgs e)
		{
			e.Cancel = CheckModified() == false;
		}

		private void lstClips_SelectedValueChanged(object sender, System.EventArgs e)
		{
			if(lstClips.SelectedItem != null)
			{
				_changing = true;
				Clip theClip = (Clip)lstClips.SelectedItem;
				txtClipText.Text = theClip.Content;
				txtClipName.Text = theClip.Name;
				_changing = false;
			}

			enableButtons(_editing);
		}

		private void txtClipName_TextChanged(object sender, System.EventArgs e)
		{
			maybeBeginEdit();
		}

		void maybeBeginEdit()
		{
			if(lstClips.SelectedItem != null && lstClips.Enabled && !_changing)
			{
				enableButtons(true);
			}
		}

		private void btnUpdate_Click(object sender, System.EventArgs e)
		{
			if(lstClips.SelectedItem != null && !lstClips.Enabled)
			{
				Clip theClip = (Clip)lstClips.SelectedItem;
				
				foreach(Clip otherClip in _clips.TextClips)
				{
					if(otherClip != theClip && otherClip.Name == txtClipName.Text)
					{
						errorProvider.SetIconAlignment(txtClipName, ErrorIconAlignment.MiddleLeft);
						errorProvider.SetError(txtClipName, "Must have a unique clip name");
						return;
					}
				}
				
				theClip.Name = txtClipName.Text;
				theClip.Content = txtClipText.Text;
			
				errorProvider.SetError(txtClipName, "");
	
				enableButtons(false);

				// Force reset of clip data...
				lstClips.Items[lstClips.SelectedIndex] = theClip;

				_clips.Modified = true;
			}
		}

		private void btnDiscard_Click(object sender, System.EventArgs e)
		{
			if(lstClips.SelectedItem != null && !lstClips.Enabled)
			{
				_changing = true;
				Clip theClip = (Clip)lstClips.SelectedItem;
				txtClipText.Text = theClip.Content;
				txtClipName.Text = theClip.Name;
				enableButtons(false);
				_changing = false;
			}
		}

		private void itmOpen_Click(object sender, System.EventArgs e)
		{
			if (CheckModified() == true &&
				openFileDialog.ShowDialog(this) == DialogResult.OK)
			{
				_changing = true;
				lstClips.Items.Clear();
				enableButtons(false);
				txtClipName.Text = "";
				txtClipText.Text = "";
				setName.Text = "";
				_filename = "";
				_clips = null;

				Clips clips = null;
				try
				{
					clips = _cs.Load(openFileDialog.FileName);
				}
				catch(Exception)
				{
				}

				if(clips != null)
				{
					_clips = clips;
					foreach(Clip c in _clips.TextClips)
					{
						lstClips.Items.Add(c);
					}
					
					setName.Text = clips.Name;

					_filename = openFileDialog.FileName;
				}				

				_changing = false;

				enableButtons(false);
			}
		}

		bool haveClip(string name)
		{
			foreach(Clip c in _clips.TextClips)
			{
				if (c.Name == name)
					return true;
			}
			
			return false;
		}

		string getGoodNewClipName()
		{
			int clip = 0;
			while(true)
			{
				string clipname = "<new clip";
				if(clip != 0)
				{
					clipname += " ";
					clipname += clip.ToString();
				}
				clipname += ">";
				
				if(!haveClip(clipname))
					return clipname;

				clip++;
			}
		}

		private void btnAdd_Click(object sender, System.EventArgs e)
		{
			if(_clips == null)
				_clips = new Clips();

			Clip c = new Clip(getGoodNewClipName());
			_clips.TextClips.Add(c);
			lstClips.Items.Add(c);
			lstClips.SelectedItem = c;
		}

		private void setName_TextChanged(object sender, System.EventArgs e)
		{
			if(_clips == null)
				_clips = new Clips();
			_clips.Name = setName.Text;
		}

		private void itmSaveAs_Click(object sender, System.EventArgs e)
		{
			if (saveFileDialog.ShowDialog(this) == DialogResult.OK)
			{
				_filename = saveFileDialog.FileName;
				Save(false);
			}
		}

		private void itmExit_Click(object sender, System.EventArgs e)
		{
			Close();
		}

		private void btnRemove_Click(object sender, System.EventArgs e)
		{
			if(lstClips.SelectedItem != null)
			{
				lstClips.Items.Remove(lstClips.SelectedItem);
				txtClipName.Text = "";
				txtClipText.Text = "";
			}
		}

		void enableButtons(bool editing)
		{
			_editing = editing;
			lstClips.Enabled = !editing && _clips != null;
			btnAdd.Enabled = !editing;
			btnRemove.Enabled = !editing && lstClips.SelectedItem != null;

			btnUpdate.Enabled = editing;
			btnDiscard.Enabled = editing;

			itmSaveAs.Enabled = _clips != null && !_editing;
			itmSave.Enabled = !_editing && _clips != null && _filename != "" && _filename != null;

			if (editing == true)
			{
				txtClipText.Focus();
			}
			else
			{
				lstClips.Focus();
			}
		}

		private void itmSave_Click(object sender, System.EventArgs e)
		{
			if(_filename != "" && _filename != null && _clips != null)
			{
				Save(false);
			}
		}

		private void itmNew_Click(object sender, System.EventArgs e)
		{
			if (CheckModified() == true)
			{
				_changing = true;
				lstClips.Items.Clear();
				txtClipName.Text = "";
				txtClipText.Text = "";
				setName.Text = "";
				_filename = "";
				_clips = null;
				_changing = false;

				enableButtons(false);
			}
		}

		private bool CheckModified()
		{
			bool res = true;
			if (_clips != null && _clips.Modified == true)
			{
				DialogResult dr = MessageBox.Show(this,
					String.Format("Save changes to {0}?", _filename),
					this.Text, MessageBoxButtons.YesNoCancel,
					MessageBoxIcon.Question,
					MessageBoxDefaultButton.Button1);
				if (dr == DialogResult.Yes)
				{
					res = Save(false);
				}
				else if (dr == DialogResult.Cancel)
				{
					res = false;
				}
			}
			return res;
		}

		private bool Save(bool forceSaveAs)
		{
			if ((_filename != "" && _filename != null) || forceSaveAs == true)
			{
				if (_filename.StartsWith("<") == true || forceSaveAs == true)
				{
					if (saveFileDialog.ShowDialog(this) == DialogResult.OK)
					{
						_filename = saveFileDialog.FileName;
					}
					else
					{
						return false;
					}
				}

				CheckPath(_filename);

				try
				{
					_cs.Save(_clips, _filename);
					_clips.Modified = false;
				}
				catch (Exception e)
				{
					//TODO: Check for read-only files
					// file:///P:/Tools/Vista%20UX%20Guide/Windows/DialogBoxes/DialogBoxesPatterns.htm#QuestionDialogsLinks
					MessageBoxExResult res = MessageBoxEx.Show(
						this,
						this.Text,
						"An error occured while saving.",
						e.Message,
						"Save as...",
						"Retry",
						MessageBoxIcon.Error);
					if (res == MessageBoxExResult.Commit1)
					{
						Save(true);
					}
					else if (res == MessageBoxExResult.Commit2)
					{
						Save(false);
					}
					return res != MessageBoxExResult.Cancel;
				}
			}
			return true;
		}

		/// <summary>
		/// Checks whether the specified filename is located in a path visible
		/// to PN.
		/// </summary>
		/// <param name="filename">The filename the check.</param>
		void CheckPath(string filename)
		{
			string userDataPath = GetUserDataPath();
			string path = Path.GetFullPath(filename);
			if (path != userDataPath)
			{
				string msg = string.Format(
					"Saving text clips to a location different from\n{0}\nmakes them unavailble to Programmer's Notepad.",
					userDataPath);

				MessageBox.Show(this, msg, Text, MessageBoxButtons.OK,
					MessageBoxIcon.Warning);
			}
		}

		/// <summary>
		/// Retrieves the path to the PN2 directory in the users application
		/// data folder.
		/// </summary>
		/// <returns>The path.</returns>
		static string GetUserDataPath()
		{
			return Path.Combine(
				Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
				UserDataDirectory);
		}
	}
}
