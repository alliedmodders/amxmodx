In deps.zip are the dependencies needed to build the AMXX installer project in Delphi 7 on Windows.

For getting the dependencies, see https://wiki.alliedmods.net/Building_AMX_Mod_X

Follow the directions below in order to install these:

1)  Make sure the Delphi IDE has been run at least once. Then make sure that the Delphi IDE is
    closed before trying to install anything.

2)  Decompress the zip file and make sure the following are available:

    flatstyle\
    indy9\
    jcl\
    jvcl\
    mxFlatPack\
    madCollection.exe

3)  Run the madCollection.exe installer and make sure to click on "madExcept4" before clicking the
    Install button. After Install has been clicked, accept the license agreement and click Continue
    Type "yes" in the textbox that appears next and click Continue. Finally, click the Install
    button. After installation is complete, the components should appear in Delphi.
   
4)  Open the jcl folder and run Install.bat. Click the MPL 1.1 License tab and make sure to accept
    the license. Then click the Install button and click Yes for any questions that are asked.
    This will compile and install the JCL library which will appear when the Delphi IDE is next
    opened.

5)  Open the jvcl folder and run install.bat. Click the Next button a number of times until it
    changes into the Install button. The default settings will suffice, so now click the Install
    button. This will install the JVCL library which will appear when the Delphi IDE is next
    opened.
   
6)  Open the mxFlatPack folder and the Component subfolder. Open mxFlatPack_D7.dpk. This should
    open the Delphi IDE. An error about not finding a resource file may appear, but this can be
    ignored. Click the Install button in the Package window. 
   
7)  When this is complete, go to Tools -> Environment Options on the menu and click the Library tab.
    Click the ... button next to where it says "Library path". On the next window, click the ...
    button. Locate the mxFlatPack\Component folder in the Browse window and click OK. Click the Add
    button followed by the OK button. Click OK one more time and close the Delphi IDE.
   
8)  Open the indy9 folder. Open dclIndy70.dpk. This should open the Delphi IDE. An error about not
    finding a resource file may appear, but this can be ignored. Click the Install button in the
    Package window. An error about a package that can't be installed will appear, but this can be
	ignored. Click OK on the error message and click the Install button a second time.

9)  When this is complete, go to Tools -> Environment Options on the menu and click the Library tab.
    Click the ... button next to where it says "Library path". On the next window, click the ...
    button. Locate the indy9 folder in the Browse window and click OK. Click the Add button
    followed by the OK button. Click OK one more time and close the Delphi IDE.
   
10) Open the flatstyle folder and open the Packages subfolder. Open FlatStyle_D6.dpk. This should
    open the Delphi IDE. Close the FlatStyle_D6.dpk document window. Click the Install button in
    the Package window.
   
11) When this is complete, go to Tools -> Environment Options on the menu and click the Library tab.
    Click the ... button next to where it says "Library path". On the next window, click the ...
	button. Locate the flatstyle\Source folder in the Browse window and click OK. Click the Add
	button followed by the OK button. Click OK one more time and close the Delphi IDE.

At this point, all the dependent components should be installed and the AMXX installer project,
AMXInstaller.dpr, can now be built.
