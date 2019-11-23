Gui, Add, Text,, Please enter your name:

Gui, Add, Text,, "lel"
Gui, Add, Button, Default, &akkli
Gui, Add, Text,, "top"
Gui, Add, Button,, &lenny
Gui, Add, Button,, Cancel

Gui, Show
return


Buttonakkli:
    clipboard := "https://akk.li/pics/anne/jpg"
    ExitApp
return
Buttonlenny:
    clipboard := "( ͡° ͜ʖ ͡°)"
    ExitApp
return

ButtonCancel:
ExitApp