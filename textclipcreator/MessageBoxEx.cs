using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace TextClipCreator
{
	/// <summary>
	/// Summary description for MessageBoxExResult.
	/// </summary>
	public enum MessageBoxExResult
	{
		Commit1,
		Commit2,
		Cancel
	};

	/// <summary>
	/// Summary description for MessageBoxEx.
	/// </summary>
	public class MessageBoxEx : System.Windows.Forms.Form
	{

		private System.Windows.Forms.PictureBox pictureBox;
		private System.Windows.Forms.Label lblInstruction;
		private System.Windows.Forms.Label lblDetails;
		private System.Windows.Forms.Button btnCommit1;
		private System.Windows.Forms.Button btnCommit2;
		private System.Windows.Forms.Button btnCancel;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		internal MessageBoxEx(String instruction, String details, String commit1, String commit2, MessageBoxIcon icon)
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			lblInstruction.Text = instruction;
			lblDetails.Text = details;
			btnCommit1.Text = commit1;
			btnCommit2.Text = commit2;
			switch (icon)
			{
				case MessageBoxIcon.Asterisk:
					pictureBox.Image = (Image)SystemIcons.Asterisk.ToBitmap();
					break;
				case MessageBoxIcon.Error:
					pictureBox.Image = (Image)SystemIcons.Error.ToBitmap();
					break;
				case MessageBoxIcon.Exclamation:
					pictureBox.Image = (Image)SystemIcons.Exclamation.ToBitmap();
					break;
				case MessageBoxIcon.Question:
					pictureBox.Image = (Image)SystemIcons.Question.ToBitmap();
					break;
				default:
					break;
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
			this.pictureBox = new System.Windows.Forms.PictureBox();
			this.lblInstruction = new System.Windows.Forms.Label();
			this.lblDetails = new System.Windows.Forms.Label();
			this.btnCommit1 = new System.Windows.Forms.Button();
			this.btnCommit2 = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// pictureBox
			// 
			this.pictureBox.Location = new System.Drawing.Point(16, 38);
			this.pictureBox.Name = "pictureBox";
			this.pictureBox.Size = new System.Drawing.Size(32, 32);
			this.pictureBox.TabIndex = 0;
			this.pictureBox.TabStop = false;
			// 
			// lblInstruction
			// 
			this.lblInstruction.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.lblInstruction.Location = new System.Drawing.Point(16, 8);
			this.lblInstruction.Name = "lblInstruction";
			this.lblInstruction.Size = new System.Drawing.Size(288, 20);
			this.lblInstruction.TabIndex = 1;
			this.lblInstruction.Text = "Are you sure you want to ...?";
			// 
			// lblDetails
			// 
			this.lblDetails.Location = new System.Drawing.Point(64, 38);
			this.lblDetails.Name = "lblDetails";
			this.lblDetails.Size = new System.Drawing.Size(244, 54);
			this.lblDetails.TabIndex = 2;
			this.lblDetails.Text = "Details go here";
			// 
			// btnCommit1
			// 
			this.btnCommit1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnCommit1.DialogResult = System.Windows.Forms.DialogResult.Retry;
			this.btnCommit1.Location = new System.Drawing.Point(67, 114);
			this.btnCommit1.Name = "btnCommit1";
			this.btnCommit1.TabIndex = 3;
			this.btnCommit1.Text = "Commit 1";
			// 
			// btnCommit2
			// 
			this.btnCommit2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnCommit2.DialogResult = System.Windows.Forms.DialogResult.Ignore;
			this.btnCommit2.Location = new System.Drawing.Point(151, 114);
			this.btnCommit2.Name = "btnCommit2";
			this.btnCommit2.TabIndex = 4;
			this.btnCommit2.Text = "Commit 2";
			// 
			// btnCancel
			// 
			this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Location = new System.Drawing.Point(235, 114);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "Cancel";
			// 
			// MessageBoxEx
			// 
			this.AcceptButton = this.btnCommit1;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(318, 145);
			this.ControlBox = false;
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnCommit2);
			this.Controls.Add(this.btnCommit1);
			this.Controls.Add(this.lblDetails);
			this.Controls.Add(this.lblInstruction);
			this.Controls.Add(this.pictureBox);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Name = "MessageBoxEx";
			this.ShowInTaskbar = false;
			this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "MessageBoxEx";
			this.ResumeLayout(false);

		}
		#endregion

		static public MessageBoxExResult Show(Form owner, String caption, String instruction, String details, String commit1, String commit2, MessageBoxIcon icon)
		{
			MessageBoxEx mb = new MessageBoxEx(instruction, details, commit1, commit2, icon);
			mb.Text = caption;
			MessageBoxExResult res;
			switch (mb.ShowDialog(owner))
			{
				case DialogResult.Retry:
					res = MessageBoxExResult.Commit1;
					break;
				case DialogResult.Ignore:
					res = MessageBoxExResult.Commit2;
					break;
				case DialogResult.Cancel:
					res = MessageBoxExResult.Cancel;
					break;
				default:
					res = MessageBoxExResult.Commit1;
					break;
			}
			return res;
		}
	}
}
