using System;
using Borland.Studio.ToolsAPI;

///THis unit implements a base class for add ins
namespace MarcRohloff.BDS.Utilities
{
	public abstract class BDSAddInBase : IDisposable
	{
	  protected BDSAddInBase() {}

      /*This method must be called from main or IDERegister to start the AddIn*/
      public void Start()
      {
       try
       {
         if (BDSInterop.StandAlone)
           RunStandalone();
         else
           Register();
       }
       catch (Exception e)
       {
         System.Windows.Forms.MessageBox.Show(e.ToString() );
       }
      }

      #region must be overriden
      protected abstract string Name
        { get; }

      protected abstract string Description
        { get; }

      protected abstract System.Drawing.Bitmap Bitmap
        { get; }

      protected virtual void Register()
      {
        wizard = new WizardLink(this);
        BDSInterop.AddWizard  ( (IOTAWizard)wizard );
        BDSInterop.AddAboutBox( (IOTAWizard)wizard, Name, Description, Bitmap);
      }

      #endregion must be overridden

      #region Can be overridden

      protected virtual string IDString
        { get { return BDSInterop.MakeValidName(GetType().Namespace); } }

      protected virtual void RunStandalone() {}

      protected virtual void UnRegister()
      {
        foreach (string n in menuItems)
          RemoveMenuItem(n);

        BDSInterop.RemoveAboutBox( (IOTAWizard)wizard);
        BDSInterop.RemoveWizard  ( (IOTAWizard)wizard);
      }

      protected virtual void Dispose(bool canDisposeManaged)
      {
        if (canDisposeManaged)
        try
        {
          UnRegister();
        }
        catch (Exception e)
        {
          System.Windows.Forms.MessageBox.Show(e.ToString() );
        }
      }
      #endregion can be overridden

      #region Menu functions (protected)
      public IOTAMenuItem AddMenuItem(string relativeTo,
                                             BDSMenus.Position locn,
                                             string menuName,
                                             string menuCaption,
                                             EventHandler action,
                                             System.Drawing.Bitmap     bitmap,
                                             System.Windows.Forms.Keys shortCut)
      {
        if (menuName=="")
          menuName = IDString + '_' + menuItemsCount.ToString();
        menuItemsCount++;

        menuName = String.Intern(menuName);
        if (menuItems.Contains(menuName))
          throw new BDSException("A menu item named " + menuName + " already exists for wizard " + Name);

        IOTAMenuItem i = BDSMenus.AddMenuItem(relativeTo, locn,
                                              menuName, menuCaption,
                                              action, bitmap, shortCut);
        menuItems.Add(menuName);
        return i;
      }

      public IOTAMenuItem AddMenuSeperator(string relativeTo, BDSMenus.Position locn)
      {
        return AddMenuItem(relativeTo, locn, "", "-", null, null, 0);
      }

      public void RemoveMenuItem(IOTAMenuItem item)
        {RemoveMenuItem(item.Name);}

      public void RemoveMenuItem(string menuName)
      {
        menuName = String.Intern(menuName);
        menuItems.Remove(menuName);
        BDSMenus.RemoveMenuItem(menuName);
      }

      #endregion Menu functions (protected)

      #region IOTAWizard nested link class
      /*Used a second class so that subclasses of BDSAddIn
        don't have a dependency on B.S.ToolsAPI   */
      private class WizardLink : IOTAWizard
      {
        internal WizardLink(BDSAddInBase owner) {this.owner = owner; }

        public void Execute()     { /*Not called*/  }

        public void Destroyed()   { /* Not called*/ }

        public string IDString
          { get { return owner.IDString; } }

        public string Name
          { get {return owner.Name; } }

        private BDSAddInBase owner;
      }
      #endregion IOTAWizard nested link class

      #region IDisposable implementation
      //Note I implemented a slightly different and I hope improved
      // version of the IDisposable pattern here which elimates
      //  The repeated nested if's in the Dispose(bool) method
      //
      // If a finalizer is used it should simply be coded as
      //  Dispose(false)
      public void Dispose()
      {
        if(!disposed)
          Dispose(true);
        disposed = true;
        GC.SuppressFinalize(this);
      }

      #endregion IDisposable implementation

      #region private methods
      #endregion private methods

      #region private fields
      bool       disposed = false;
      int        menuItemsCount = 0;
      object     wizard   = null;   //Really IOTAWizard but stored as object to remove dependancy on B.S.ToolsAPIU
      System.Collections.ArrayList menuItems = new System.Collections.ArrayList();
      #endregion private fields
	}
}
