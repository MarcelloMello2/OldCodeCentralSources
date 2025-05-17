 Author  : Victor Cheryaev
 Email   : Cheryaev@tut.by
 Relesed :  5th March 2005

Extract files this zip. Open TlsLoadPack.bpg and you find two projects: SampleHost.dpr and Plugin.dpk. 
First compile Plugin.dpk to create runtime package Plugin.bpl, then compile SampleHost.dpr and Run SampleHost.exe.

This Sample demonstrates how to use dynamic loading Package as a Plugin and how to use interface type and Tls (Thread Local 
Storage) to provide interaction between these Host and Plugin. 

Advantages of such approach: 
1) All advantages of "application with dynamic plugin system" plus advantages of using BPL (Borland Package Library). Host 
and Plugin projects both may be compiled "Build with runtime package" with commonly-used "rtl;vcl" packages and so reduce 
Plugin's code.
2) Host's and Plugin's projects have only one together-shared unit in their Uses lists. That unit contains interface declarations 
and additional Tls code. Interface Type provides independence of application's and Plugin's development. 

Features: 
As process's TLS slot with an index 0 is used for storing newlly allocated Tls's indexes, it is necessary to take into consideration 
the order of creating of interface'  inplementing objects and calling them method. For example, first we must create instance of 
interface's implementing class and call our procedure SetTls in Plugin, than we call GetTls before any method of that interface 
calls in Host. There should not be no others calls SetTls between these SetTls and GetTls. 
To avoid this restriction, it is necessary a little to change a code, but here approach is considered only.

You can also unzip another example - Intro.zip, which has almost the same code except additional Tls-provided code. It's only for 
introducing in a problem. We can see that it is easy to use load-time linking dynamic library as a Plugin, but for run-time linking 
it is nessesary to provide additional code.
