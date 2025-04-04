# Project Specification Document

### Project Name: Classroom Route Finder 
### Team Members: Kini & JD
### Course: COMP 3649
### Date: April 4th 2025

## Introduction
### Purpose: The Classroom Route Finder (CRF) is a Java and Haskall based application designed to help students navigate campus buildings efficiently. It provides:
* Time constrained routes to classrooms
* Accessibility aware paths (elevators, avoiding stairs)
* Text based directions using compass navigation

### Scope: 
* Initial implementation for Bisset Business (EB) building (all floors)
* Supports inputs: starting/destination rooms, time limit, mobility constraints
* Outputs step by step directions or error messages
* Only classes we deemed as actual classrooms were included (ex. storage closets and offices were omitted)

## Functional Requirements
### User Inputs
|Parameter     | Format              | Validation                          |
|--------------|---------------------|-------------------------------------|
|Starting Room | String (ex. EB1113) |Must exists in the building's dataset|
|Destination Room | String (ex. EB2138) |Must exists in the building's dataset|
|Time Constraint | Integer (Minutes) |Rejects if not in the range 1 ≤ t ≤ 6|
|Mobility Constraint | Boolean (TRUE/FALSE) |True enables elevator only routes, false enables stairs only routes|

### Example Command:
```
Java version:
java Main EB1207 EB3204 2 FALSE
Haskell version:
**------------------------**
```


# Core Logic
* ### **Pathfinding Algorithm:** Depth First Search (DFS) with back tracking for:
* Dead ends (Stairs when mobility = TRUE)
* Exceeding time constraints
* ### **Time Calculation:**
* 1 meter = 1 second
* Stairs and elevators cost 30 seconds between floors
* Distances derived from GIS technology (jk using a ruler against the monitor as well as google maps measure tool)

# Output Format
#### Success Example:
```
Navigating from Classroom EB1113 to Classroom EB2138 with a time constraint of 1 minutes and mobility constraint: false

                Route Directions:

-Go NE towards Classroom: EB1113 at 28m along hallwayEB1_SW_NE

-Arrive at Intersection: bisset_NorthEast_Intersection_F1
*From here:
-Take Stairs: stairsEB03_F1 at intersection: bisset_NorthEast_Intersection_F1
-Take Stairs: stairsEB03_F2 at intersection: bisset_NorthEast_Intersection_F2

-Arrive at Intersection: bisset_NorthEast_Intersection_F2
*From here:
-Go NE towards Classroom: EB2138 at 50m along hallwayEB2_SW_NE

You have arrived. Total time taken: 0 minutes and 52 seconds.
```
### Error Cases:
* Invalid room: Classroom does not exist
* No route: No accessible route available within timeframe

# Technical Specifications
## System Architecture
* Language: Java & Haskall
* Data Structures: Graph(nodes = classrooms/intersections/stairs/elevators; edges = hallways)
* External Data: Building modelling stored in input text file
## Assumptions
* User starts adjacent to a valid classroom

# Non-Functional Requirements
|Category     | Requirement             | 
|--------------|---------------------|
|Performance | Responds within 3 seconds for valid inputs |
|Usability | Text output readable via CLI |
|Scalability |Supports adding new buildings incrementally|

# Heuristics
* **6 mins upper time constraint limit:** chosen because it should not take more than that to walk between any 2 classes in the Bisset building without distractions
* **Coordinate System:** Based on actual coordinates but specific classrooms were assigned a quadrant based on the vibes/what we felt they were closer to (NW/SE/SW/NE)
* **Time to cover 1 meter:** One second was chosen to cover one meter because upon Kini's request JD personally tested this metric during the tutorial and it seemed legit

# Limitations or Bugs

* The program does not find the shortest path, it only finds a path.
    * All the nodes are sorted by position on a hallway and the path finding algorithm prioritizes nodes with a lower value, therefore it will alway explore that direction there which could lead to longer than necessary paths unless the time constraint is minimized.
* Only routes between classrooms are implemented and not other spaces such as washrooms and water fountains.
* The program cannot be made aware easily of any dynamic obstacles in the future such as construction work blocking certain paths


# Project Outcomes 
* Graph Theory: DFS traversal with weighted edges
* Data Structures: Custom Node class, graph representation
* Imperative programming: Java implementation
* Declarative Programming: Haskall implementation










