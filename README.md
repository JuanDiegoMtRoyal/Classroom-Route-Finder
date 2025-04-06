# Classroom-Route-Finder
 An imperative and declarative solution for finding a path between classrooms at MRU given certain constraints. Implemented in Java and Haskell 

 # JAVA
 * The first step is to make sure you have a JDK installed. If you are using visual studio code download the Microsoft java extension pack. Once you have the Classroom Route Finder folder open, enter the src folder and type ```javac *.java``` to compile everything. Then run the program by entering ```java Main startclassroom endclassroom timeconstraint(int 1-6) mobilityconstraint(bool true/false)``` at the prompt, with the classroom parameters replaced by any classroom in the building text file that is located in the data folder.

# HASKELL
* To run the Haskell version, ensure you have GHC properly installed. We used ghcup to download everything automatically. Then download the Haskell extension on the visual studio code marketplace. Once that is done head into the haskell folder and input ``` ghc main ``` at the command line to compile the program. Then execute it by typing ``` ./main <startClass> <endClass> <timeConstraint> <mobilityConstraint> ```

