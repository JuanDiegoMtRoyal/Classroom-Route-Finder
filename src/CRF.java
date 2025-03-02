import java.io.*;
import java.util.*;

public class CRF {

    private Graph graph;

    public CRF()
    {
        graph = new Graph();
        //initializeBuildingMap("bissetFloor1.txt"); // Populate the graph with nodes and hallways
        Parser.initializeBuildingMap("bisserFloor1.txt");

    }

    private void initializeBuildingMap(String filename)
    {
        try(BufferedReader reader = new BufferedReader(new FileReader(filename)))
        {
            String line;
            String currentSection = null;
            while((line = reader.readline()) != null)
            {
                line = line.trim();
                if(line.isEmpty())
                {
                    continue; 
                }

                if(line.startsWith("#"))
                {
                  if(line.contains("Hallway Format"))
                  {
                    currentSection = "Hallway";
                  }  
                  else if(line.contains("Classroom Format"))
                  {
                    currentSection = "Classroom";
                  }
                  else if(line.contains("Stairs Format"))
                  {
                    currentSection = "Stairs";
                  }
                  else if(line.contains("Elevator Format"))
                  {
                    currentSection = "Elevator";
                  }
                  else if(line.contains("Intersection Format"))
                  {
                    currentSection = "Intersection";
                  }
                  continue;
                }

                switch(currentSection)
                {
                    case "Hallway":
                    parseHallway(line);
                    break;
                    case "Classroom":
                    parseClassroom(line);
                    break;
                case "Stairs":
                    parseStairs(line);
                    break;
                case "Elevator":
                    parseElevator(line);
                    break;
                case "Intersection":
                    parseIntersection(line);
                    break;
                default:
                    // Unknown section, skip the line
                    break;
                }
            }


        }
        catch (IOException e) 
        {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }




    
}
