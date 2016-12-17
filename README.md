# Logic Compiler

![alt=Logic Compiler](https://cloud.githubusercontent.com/assets/1095436/21286569/805040de-c436-11e6-9d09-5434fcd650e7.png)
![alt=Logic Compiler](https://cloud.githubusercontent.com/assets/1095436/21286570/846527f2-c436-11e6-8b3f-dadd73995002.png)

Minimum requirements
=
    - at least JVM 1.8
    - Python 2

How to use
=
Download the [latest stable realease](https://github.com/mariohd/LogicCompiler/releases) and run the JAR file.

Grammar
=
    proposition ::= premise | (operation)
    operation   ::= ~ proposition | proposition ^ proposition | proposition v proposition | proposition -> proposition
    premise     ::= [A-Z]

The project
=
This was an assignment for Formal Aspects of Computing (Aspectos Formais da Computação) of UNIFOR's PPGIA - Master degree in Applied Informatics Graduate Program.