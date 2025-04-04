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
| **Same Floor Navigation** <br>(Same hallway) |Java Main EB1108 EB1102 <BR> hASKELL INPUT           |                        |                        |                    |  
| **Same Floor Navigation** <br>(Different hallway) |Java Main EB1108 EB1102 <BR> hASKELL INPUT           |                        |                        |                    |  

**Different Floor Navigation**
|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Different Floor Navigation** |Java Main EB1108 EB1102 <BR> hASKELL INPUT           |                        |                        |                    |  
| **Different Floor Navigation**  <br>(Long Route with **normal time** constraint) |Java Input <BR> hASKELL INPUT           |                        |                        |                    |  
| **Different Floor Navigation**  <br>(Long Route with **short time** constraint) |Java Input <BR> hASKELL INPUT           |                        |                        |                    |  
| **Different Floor Navigation**  <br>(Long Route with **miniscule time** constraint) |Java Input <BR> hASKELL INPUT           |                        |                        |                    |  

### Mobility Constraint Cases (True)
 
|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Same Floor Navigation** <br>(Different hallway) |Java Main EB1108 EB1102 <BR> hASKELL INPUT           |                        |                        |                    |  
| **Different Floor Navigation**  <br>(Long Route with **normal time** constraint) |Java Input <BR> hASKELL INPUT           |                        |                        |                    |  

### Edge Cases

|Test Case                                     |Input                                                |Expected                |Java Output             | Haskell Output     |
|----------------------------------------------|-----------------------------------------------------|------------------------|------------------------|--------------------|
| **Same Start and End Classroom**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Minimum Time Constraint (1)**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Maximum Time Constraint (6)**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Classroom does not exist**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Invalid time  (0)**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Invalid time  (7)**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Invalid time  (-1)**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 
| **Extreme Opposite Ends of Building**  |Java  <BR> hASKELL INPUT           |                        |                        |                    | 

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







