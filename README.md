# Classroom-Route-Finder
 An imperative and declarative solution for finding a path between classrooms at MRU given certain constraints. Implemented in Java and Haskell 

 # JAVA
 * The first step is to make sure you have a JDK installed. If you are using visual studio code download the Microsoft java extension pack. Once you have the Classroom Route Finder folder open, enter the src folder and type ```javac *.java``` to compile everything. Then run the program by entering ```java Main startclassroom endclassroom timeconstraint(int 1-6) mobilityconstraint(bool true/false)``` at the prompt, with the classroom parameters replaced by any classroom in the building text file that is located in the data folder.

# HASKELL
* To run the Haskell version, you must ensure you have ghc properly installed. Running from the GHC has not yet been tested so it is advised to run the program from GHCi. Open the haskell folder and find ```Main.hs``` file, open this with GHCi and it will begin the compilation of the program. Run the command ```main``` and you should be prompted with a blinking pointer on the next line, allowing the user input ```startClassroom endClassroom timeConstraint(Int 1-6) mobilityConstraints(bool true/false)``` with the classroom parameters replaced by any classroom in the building text file located in the data folder.
