using System;

namespace MarcRohloff.FavouritesMenuAddIn
{
	public class FavouritesStartup
	{
		private FavouritesStartup() {} /* Static class */

        //DLL Add-in Assemblies use the static IDERegister method as the entry point
		public static void IDERegister()
		{ new FavouritesMenuAddIn().Start(); }

		//EXE Add-In Assemblies use the static Main method as the entry point
		[STAThread]
		public static void Main()
		{ new FavouritesMenuAddIn().Start(); }

	}
}

