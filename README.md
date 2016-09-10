# Logic Compiler

![alt=Logic Compiler](https://cloud.githubusercontent.com/assets/1095436/18411313/3d19c48a-774b-11e6-9744-b64a471b16d1.png)

Minimum requirements
=
    - at least JVM 1.8

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