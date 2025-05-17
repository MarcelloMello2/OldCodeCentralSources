using System;
using System.Windows.Forms;

namespace MarcRohloff.AddInExpert
{
	public class AddInStartup
	{
	   private AddInStartup() {} /*static class*/

        //DLL Add-in Assemblies use the static IDERegister method as the entry point
		public static void IDERegister()
		{ new AddInExpertAddIn().Start(); }

		//EXE Add-In Assemblies use the static Main method as the entry point
		[STAThread]
		public static void Main()
        { new AddInExpertAddIn().Start(); }

	}
}
