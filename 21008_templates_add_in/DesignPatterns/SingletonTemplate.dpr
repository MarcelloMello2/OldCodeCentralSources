program [!ProgramName];

// Created with USysWare Templates add-in ([!TimeStamp])

{$APPTYPE CONSOLE}

var
  Singleton1, Singleton2: TSingleton;

begin
  Console.WriteLine('Create a first object...');
  Singleton1 := TSingleton.GetSingleton;
  Console.WriteLine('Number of instances of ' + TSingleton.ClassName +
    ' type is ' + Singleton1.Instance.ToString);
  Console.WriteLine;

  Console.WriteLine('Create a second object...');
  Singleton2 := TSingleton.GetSingleton;
  Console.WriteLine('Number of instances of ' + TSingleton.ClassName +
    ' type is ' + Singleton2.Instance.ToString);
  Console.WriteLine;

  Console.WriteLine('Press Enter to end...');
  Console.ReadLine;
end.