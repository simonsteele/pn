using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace ProjectTemplateEditor
{
	/// <summary>
	/// Summary description for TextInputForm.
	/// </summary>
	public class TypeInputForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Button btnOK;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.ListBox listBox;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public TypeInputForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
		}

		public ValueType Result
		{
			get
			{ 
				if(listBox.Text == "Text")
					return ValueType.Text;
				else if(listBox.Text == "Boolean (True/False)")
					return ValueType.Boolean;
				else if(listBox.Text == "Option List")
					return ValueType.OptionList;
				else if(listBox.Text == "File Browser")
					return ValueType.FilePath;
				else if(listBox.Text == "Folder Browser")
					return ValueType.FolderPath;
				else if(listBox.Text == "Integer")
					return ValueType.Integer;
				else throw new ApplicationException("Unexpected listbox value");
			}
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
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
			this.btnOK = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.label1 = new System.Windows.Forms.Label();
			this.listBox = new System.Windows.Forms.ListBox();
			this.SuspendLayout();
			// 
			// btnOK
			// 
			this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnOK.Enabled = false;
			this.btnOK.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnOK.Location = new System.Drawing.Point(184, 120);
			this.btnOK.Name = "btnOK";
			this.btnOK.Size = new System.Drawing.Size(72, 24);
			this.btnOK.TabIndex = 2;
			this.btnOK.Text = "&OK";
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
			this.btnCancel.Location = new System.Drawing.Point(104, 120);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(72, 24);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Cancel";
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(16, 16);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(48, 16);
			this.label1.TabIndex = 0;
			this.label1.Text = "Type:";
			this.label1.TextAlign = System.Drawing.ContentAlignment.TopRight;
			// 
			// listBox
			// 
			this.listBox.Items.AddRange(new object[] {
														 "Text",
														 "Option List",
														 "Boolean (True/False)",
														 "File Browser",
														 "Folder Browser",
														 "Integer"});
			this.listBox.Location = new System.Drawing.Point(80, 16);
			this.listBox.Name = "listBox";
			this.listBox.Size = new System.Drawing.Size(176, 95);
			this.listBox.TabIndex = 4;
			this.listBox.SelectedIndexChanged += new System.EventHandler(this.listBox_SelectedIndexChanged);
			// 
			// TypeInputForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(264, 149);
			this.Controls.Add(this.listBox);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnOK);
			this.Controls.Add(this.label1);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Name = "TypeInputForm";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Property Type";
			this.ResumeLayout(false);

		}
		#endregion

		private void listBox_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			btnOK.Enabled = listBox.SelectedIndex != -1;
		}
	}
}
