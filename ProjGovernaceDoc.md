# Project Governance Document

### Project Name: Classroom Route Finder 
### Team Members: Kini & JD
### Course: COMP 3649
### Date: April 4th 2025

## Project Vision & Goals
### Objective: Develop a tool to find routes between classrooms in the Bisset Business Building, considering time constraints and mobility limitations
### Key Features:
* Pathfinding algorithm (DFS with backtracking)
* Support for stairs and elevators
* Time constrained routing
* Mobility constrained routing (elevators only)
* Clear navigation instructions

## Team Organization

* ### Roles & Responsibilities
* **Shared**(both paradigms): Code reviews, logical planning, hueristic decisions, debugging, testing, presentation preparation
*  **JD**: Imperative primary developer 
*  **Kini**: Declaritive primary developer
*  ### Communication Protocol
*  Weekly meetings (in person and Discord)
*  Discord for daily communications
*  Shared Google Drive for collaborative documents/ hashing out ideas


## Version Control & Code Management
* **System:** Github
* **Collaborative Working Environment** Live Share (on VSCode)
* **Branching Strategy:** ***main*** branch with any updates to code and creating local backups before pulling
* **Commits Conventions:** Somewhat descriptive commit messages but mainly the description of the commit was communicated through discord at the time of pushing.

## Development Process
* **Methodology:** Agile inspired iterative development
* **Workflow:**
1. Requirement Analysis (Includes walking through the building together and talking about what information we need to gather)
2. Technical Design
3. Partial Implementation
4. Code review
5. Testing/Debugging
6. Adding new changes/feature
* **Tools:**
* IDE: Visual Studio Code, GHCI, Notepad++
* Documentation: Markdown 

## Quality Assurance
 See QualityAssuranceDoc.md

## Timeline & Milestones
* ### **Key Dates:**
* **Jan 27:** Updated Concept memo sent and approved
* **Jan 31:** All objects required decided upon
* **Mar 4:** Imperative partial solution presented
* **Redacted:** Imperative solution complete
* **NA** Haskell solution complete
* **Apr 4:** Final submission

## Decision Making
* ### Process:
* Hueristics decisions: What felt reasonable
* Technical decisions: Consensus based
* ### Conflict Resolution
* Open communication through Discord
* Instructor mediation if needed


# Challenges & Solutions

| Challenges   |   Solutions |
|------------- |-------------|
|Creating objects that link to unmade objects|Using the name, link after the object is made          |
|Switching between hallways at intersections|Intersection holds nodes of classrooms in the index closest to it from hallways so it can go into the hallway(since hallway is not a node)             |
|Avoiding stairs and elevators when traversing the same floor |Can only get to those nodes through intersection and only if switching floors|
|Debugging incorrect solutions/no solution when one should exist |Adding various logs throughout the entire code |


