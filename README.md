# Instant
Project for MRJP course at the university of Warsaw

# Language 
Simple language for arithmetic operation with printing expressions

```
Prog. Program ::= [Stmt] ;
SAss. Stmt ::= Ident "=" Exp;
SExp. Stmt ::= Exp ;
separator Stmt ";" ;

ExpAdd.            Exp1   ::= Exp2 "+"  Exp1 ;
ExpSub.            Exp2   ::= Exp2 "-"  Exp3 ;
ExpMul.            Exp3   ::= Exp3 "*"  Exp4 ;
ExpDiv.            Exp3   ::= Exp3 "/"  Exp4 ;
ExpLit.            Exp4   ::= Integer ;
ExpVar.            Exp4   ::= Ident ;
coercions Exp 4;
```

# Usage 
<ul>
    <li>1. run make using Makefile </li>
    <li>2a. for JVM ./insc_jvm InstantFile.ins </li>
    <li>2b. for LVM ./insc_llvm InstantFile.ins </li>
</ul>

# Content
Project requires BNFC for parsing syntac and GHC for compilation (both available at _students_ remote server). &nbsp; \
Repository ./src contains sourcecode for project. &nbsp; \
Repository ./lib contains files from Moodle, required for correct printing of values. &nbsp; \
Repositty ./build will be temporarily created during compilation. &nbsp; 

# Legacy
Project is partly based on my previous MIM UW course - JPP. &nbsp; \
Aforementioned project is available at <https://github.com/gbzaleski/JPP-2-Lautaro-Interpreter/blob/main/Main.hs>