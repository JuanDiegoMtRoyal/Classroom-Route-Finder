# Test Plan for Classroom Route Finder (CRF)

### Project Name: Classroom Route Finder 
### Team Members: Kini & JD
### Course: COMP 3649
### Date: April 4th 2025
## Introduction 

* **Purpose:** Ensure CRF meets functional requirements (specified in the project specification document and handles edge cases)
* **Scope:** Covers unit tests, integration tests, and manual validation for path finding, input validation, and accessibility logic

## Test Environment 
* My **beast** computer and Kinis dinky laptop
* Test Data: Predefined building maps in inputtxt
* Software: Insert software



## Test Strategy


## Test Cases                                                      
### Normal Cases(No Mobility Issues)
**Same Floor Navigation**
|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Same Floor Navigation** <br>(Same hallway) |```java Main EB1108 EB1102 2 False``` <BR> hASKELL INPUT   |                        | Route Directions:<br> -Go SE towards Classroom: EB1108 at 23m along hallwayEB1_SW_NE <br> -Go SE towards Classroom: EB1107 at 17m along hallwayEB1_SW_NE <br> -Go SE towards Classroom: EB1102 at 16m along hallwayEB1_SW_NE <br> You have arrived. Total time taken: 0 minutes and 7 seconds.  |                    |  
| **Same Floor Navigation** <br>(Different hallway) |```java Main EB1109 EB1015 2 false```<BR> hASKELL INPUT           |                        |   Route Directions:<br>-Go NE towards Classroom: EB1109 at 24m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1108 at 23m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1107 at 17m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1102 at 16m along hallwayEB1_SW_NE<br>-Arrive at Intersection: bisset_South_Intersection_F1<br>*From here:<br>-Go SW towards Classroom: EB1002 at 12m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1006 at 16m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1007 at 20m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1010 at 32m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1011 at 34m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1014 at 36m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1015 at 44m along hallwayEB1_SE_NW<br>You have arrived. Total time taken: 1 minutes and 8 seconds.                     |                    |  

**Different Floor Navigation**
|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Different Floor Navigation** |```java Main EB1010 EB3134 5 false``` <BR> hASKELL INPUT           |                        | Route Directions:<br>-Go NW towards Classroom: EB1010 at 32m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1007 at 20m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1006 at 16m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1002 at 12m along hallwayEB1_SE_NW<br>-Arrive at Intersection: bisset_South_Intersection_F1<br>*From here:<br>-Take Stairs: stairsEB04_F1 at intersection: bisset_South_Intersection_F1<br>-Take Stairs: stairsEB04_F2 at intersection: bisset_South_Intersection_F2<br>-Take Stairs: stairsEB04_F3 at intersection: bisset_South_Intersection_F3<br>-Arrive at Intersection: bisset_South_Intersection_F3<br>*From here:<br>-Go SE towards Classroom: EB3108 at 8m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3112 at 18m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3120 at 22m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3126 at 38m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3130 at 42m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3134 at 49m along hallwayEB3_SW_NE<br>You have arrived. Total time taken: 2 minutes and 21 seconds.                       |                    |  
| **Different Floor Navigation**  <br>(Long Route with **normal time** constraint) |```java Main EB1113 EB2138 4 false``` <BR> hASKELL INPUT           |           |Route Directions:<br>-Go NE towards Classroom: EB1113 at 28m along hallwayEB1_SW_NE<br>-Go NE towards Classroom: EB1112 at 27m along hallwayEB1_SW_NE<br>-Go NE towards Classroom: EB1109 at 24m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1108 at 23m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1107 at 17m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1102 at 16m along hallwayEB1_SW_NE<br>-Arrive at Intersection: bisset_South_Intersection_F1<br>*From here:<br>-Take Stairs: stairsEB04_F1 at intersection: bisset_South_Intersection_F1<br>-Take Stairs: stairsEB04_F2 at intersection: bisset_South_Intersection_F2<br>-Arrive at Intersection: bisset_South_Intersection_F2<br>*From here:<br>-Go SE towards Classroom: EB2103 at 6m along hallwayEB2_SW_NE<br>-Go NE towards Classroom: EB2121 at 18m along hallwayEB2_SW_NE<br>-Go NE towards Classroom: EB2122 at 22m along hallwayEB2_SW_NE<br>-Go NE towards Classroom: EB2128 at 30m along hallwayEB2_SW_NE<br>-Go NE towards Classroom: EB2136 at 46m along hallwayEB2_SW_NE<br>-Go NE towards Classroom: EB2138 at 50m along hallwayEB2_SW_NE<br>You have arrived. Total time taken: 1 minutes and 48 seconds. |                    |  
| **Different Floor Navigation**  <br>(Long Route with **short time** constraint) |```java Main EB1113 EB2138 1 false``` <BR> hASKELL INPUT           |                        |  Route Directions:<br>-Go NE towards Classroom: EB1113 at 28m along hallwayEB1_SW_NE<br>-Arrive at Intersection: bisset_NorthEast_Intersection_F1<br>*From here:<br>-Take Stairs: stairsEB03_F1 at intersection: bisset_NorthEast_Intersection_F1<br>-Take Stairs: stairsEB03_F2 at intersection: bisset_NorthEast_Intersection_F2<br>-Arrive at Intersection: bisset_NorthEast_Intersection_F2<br>*From here:<br>-Go NE towards Classroom: EB2138 at 50m along hallwayEB2_SW_NE<br>You have arrived. Total time taken: 0 minutes and 52 seconds.                      |                    |  

### Mobility Constraint Cases (True)
 
|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Same Floor Navigation** <br>(Different hallway) |```java Main EB1109 EB1015 2 true``` <BR> hASKELL INPUT      |                        |    Route Directions:<br>-Go NE towards Classroom: EB1109 at 24m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1108 at 23m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1107 at 17m along hallwayEB1_SW_NE<br>-Go SE towards Classroom: EB1102 at 16m along hallwayEB1_SW_NE<br>-Arrive at Intersection: bisset_South_Intersection_F1<br>*From here:<br>-Go SW towards Classroom: EB1002 at 12m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1006 at 16m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1007 at 20m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1010 at 32m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1011 at 34m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1014 at 36m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1015 at 44m along hallwayEB1_SE_NW<br>You have arrived. Total time taken: 1 minutes and 8 seconds.                    |                    |  
| **Different Floor Navigation** |```java Main EB1113 EB2138 1 true``` <BR> hASKELL INPUT       |                        |  Route Directions:<br>-Go NE towards Classroom: EB1113 at 28m along hallwayEB1_SW_NE<br>-Arrive at Intersection: bisset_NorthEast_Intersection_F1<br>*From here:<br>-Take Elevator: elevatorEB02_F1 at intersection: bisset_NorthEast_Intersection_F1<br>-Take Elevator: elevatorEB02_F2 at intersection: bisset_NorthEast_Intersection_F2<br>-Arrive at Intersection: bisset_NorthEast_Intersection_F2<br>*From here:<br>-Go NE towards Classroom: EB2138 at 50m along hallwayEB2_SW_NE<br>You have arrived. Total time taken: 0 minutes and 52 seconds.                     |                    |  

### Edge Cases

|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Same Start and End Classroom**  |```java Main EB2039 EB2039 2 TRUE```  <BR> hASKELL INPUT           |                        | Error: Start Classroom must be different than Destination Classroom                       |                    | 
| **Minimum Time Constraint (1)**  |```java Main EB2039 EB3120 1 TRUE```  <BR> hASKELL INPUT           |                        |      No accessible route available.<br>No route found.          |                    | 
| **Maximum Time Constraint (6)**  |```java Main EB2039 EB3120 6 TRUE```  <BR> hASKELL INPUT           |                        |  Route Directions:<br>-Go NW towards Classroom: EB2039 at 44m along hallwayEB2_SE_NW<br>-Go NW towards Classroom: EB2029 at 32m along hallwayEB2_SE_NW<br>-Go SW towards Classroom: EB2002 at 8m along hallwayEB2_SE_NW<br>-Arrive at Intersection: bisset_South_Intersection_F2<br>*From here:<br>-Take Elevator: elevatorEB01_F2 at intersection: bisset_South_Intersection_F2<br>-Take Elevator: elevatorEB01_F3 at intersection: bisset_South_Intersection_F3<br>-Arrive at Intersection: bisset_South_Intersection_F3<br>*From here:<br>-Go SE towards Classroom: EB3108 at 8m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3112 at 18m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3120 at 22m along hallwayEB3_SW_NE<br>You have arrived. Total time taken: 1 minutes and 36 seconds.                 |                    | 
| **Classroom does not exist**  |```java Main EB1234 EB3120 2 TRUE``` <BR> hASKELL INPUT           |                        |  Classroom does not exist. ERROR with: EB1234<br>No route found.                      |                    | 
| **Invalid time  (0)**  |```java Main EB2039 EB3120 0 TRUE```  <BR> hASKELL INPUT           |                        |  Error: Time constraint must be between 1 and 6 minuets    |                    | 
| **Invalid time  (7)**  |```java Main EB2039 EB3120 7  TRUE``` <BR> hASKELL INPUT           |                        |  Error: Time constraint must be between 1 and 6 minuets    |                    | 
| **Invalid time  (-1)**  |```java Main EB2039 EB3120 -1  TRUE```  <BR> hASKELL INPUT           |                        |  Error: Time constraint must be between 1 and 6 minuets                      |                    | 
| **Extreme Opposite Ends of Building**  |```java Main EB1028 EB3135 4 FALSE```  <BR> hASKELL INPUT           |                        | Route Directions:<br>-Go NW towards Classroom: EB1028 at 56m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1023 at 52m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1022 at 48m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1015 at 44m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1014 at 36m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1011 at 34m along hallwayEB1_SE_NW<br>-Go NW towards Classroom: EB1010 at 32m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1007 at 20m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1006 at 16m along hallwayEB1_SE_NW<br>-Go SW towards Classroom: EB1002 at 12m along hallwayEB1_SE_NW<br>-Arrive at Intersection: bisset_South_Intersection_F1<br>*From here:<br>-Take Stairs: stairsEB04_F1 at intersection: bisset_South_Intersection_F1<br>-Take Stairs: stairsEB04_F2 at intersection: bisset_South_Intersection_F2<br>-Take Stairs: stairsEB04_F3 at intersection: bisset_South_Intersection_F3<br>-Arrive at Intersection: bisset_South_Intersection_F3<br>*From here:<br>-Go SE towards Classroom: EB3108 at 8m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3112 at 18m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3120 at 22m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3126 at 38m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3130 at 42m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3134 at 49m along hallwayEB3_SW_NE<br>-Go NE towards Classroom: EB3135 at 50m along hallwayEB3_SW_NE<br>You have arrived. Total time taken: 2 minutes and 46 seconds.                       |                    | 

### Special Cases
|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Same Start and End Classroom**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Multiple Path Options**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Multiple Path Options** <br> time constraint too tight for first path  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 

## Test Data Requirements
* Complete building data file
* Modeling images for verification
* Mock user input for each test case

## Verification Methods
* Manual inspection of complex routes
* Validation of error messages for edge cases
* Timing verification for constrained routes

## Notes
* The system should handle all edge cases gracefully and provide appropriate error messages when needed
* Mobility constraints should never suggest stair usage when true
* Mobility constraints should never suggest elevator usage when false
* Time constraints should be strictly enforced
* All output should include clear directions (according to modelling diagram) and estimated time

