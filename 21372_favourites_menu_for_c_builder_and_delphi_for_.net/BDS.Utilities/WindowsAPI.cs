using System;
using System.Runtime.InteropServices;

namespace MarcRohloff.BDS.Utilities
{
	internal class WindowsAPI
	{
		private WindowsAPI() {} //Static class

        [DllImport("user32.dll", SetLastError=true)]
        internal  extern static IntPtr SetParent
                   (IntPtr hWndChild, IntPtr hWndNewParent);

        [DllImport("user32.dll", SetLastError=true)]
        internal  extern static IntPtr SetWindowLong
                   (IntPtr hWnd, int nIndex, IntPtr dwNewLong);

        [DllImport("user32.dll", SetLastError=true)]
        internal extern static bool MessageBeep(UInt32 uType);

        internal const int GWL_HWNDPARENT = -8;


	}
}
