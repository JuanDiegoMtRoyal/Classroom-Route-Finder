import java.io.*;
import java.util.*;


public class Parser {
    private Graph graph;
    public Parser(Graph graph) {
        this.graph = graph;
    }

    
public void initializeBuildingMap(String filename)
    {
        try(BufferedReader reader = new BufferedReader(new FileReader(filename)))
        {
            String line;
            String currentSection = null;
            while((line = reader.readLine()) != null)
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

    private void parseHallway(String line)
    {
        String[] parts = line.split("[(),]");
        if(parts.length < 7)
        {
            System.err.println("Invalid hallway format: " + line);
            return; 
        }

        String name = parts[0].trim();
        String building = parts[1].trim();
        String startIntersectionName = parts[2].trim();
        String direction1 = parts[3].trim();
        String direction2 = parts[4].trim();
        int floor = Integer.parseInt(parts[5].trim());
        int length = Integer.parseInt(parts[6].trim());

        //create hallway
        Intersection startIntersection = (Intersection) graph.getNode(startIntersectionName);
        Hallway hallway = new Hallway(name, building, startIntersection, direction1, direction2, floor, length);
        graph.addHallway(hallway);

    }

    private void parseClassroom(String line) {
        String[] parts = line.split("[(),]");
        if (parts.length < 6) {
            System.err.println("Invalid classroom format: " + line);
            return;
        }

        String name = parts[0].trim();
        String building = parts[1].trim();
        String hallwayName = parts[2].trim();
        int position = Integer.parseInt(parts[3].trim());
        String compassDirection = parts[4].trim();
        int floor = Integer.parseInt(parts[5].trim());

        // Find the hallway
        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Classroom classroom = new Classroom(name, building, hallway, position, compassDirection, floor);
            graph.addNode(classroom);
            hallway.addNode(classroom);
        }
    }

    private void parseStairs(String line) {
        String[] parts = line.split("[(),]");
        if (parts.length < 7) {
            System.err.println("Invalid stairs format: " + line);
            return;
        }

        String name = parts[0].trim();
        String building = parts[1].trim();
        String hallwayName = parts[2].trim();
        int position = Integer.parseInt(parts[3].trim());
        String compassDirection = parts[4].trim();
        String teleportTo = parts[5].trim();
        int floor = Integer.parseInt(parts[6].trim());

        // Find the hallway
        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Stairs stairs = new Stairs(name, hallway, position, floor);
            graph.addNode(stairs);
            hallway.addNode(stairs);
        }
    }

    private void parseElevator(String line) {
        String[] parts = line.split("[(),]");
        if (parts.length < 7) {
            System.err.println("Invalid elevator format: " + line);
            return;
        }

        String name = parts[0].trim();
        String building = parts[1].trim();
        String hallwayName = parts[2].trim();
        int position = Integer.parseInt(parts[3].trim());
        String compassDirection = parts[4].trim();
        String teleportTo = parts[5].trim();
        int floor = Integer.parseInt(parts[6].trim());

        // Find the hallway
        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Elevator elevator = new Elevator(name, hallway, position, floor);
            graph.addNode(elevator);
            hallway.addNode(elevator);
        }
    }



    //NEEDS WORK what about other hallways?
    private void parseIntersection(String line) {
        String[] parts = line.split("[(),]");
        if (parts.length < 2) {
            System.err.println("Invalid intersection format: " + line);
            return;
        }

        String name = parts[0].trim();
        int floor = Integer.parseInt(parts[1].trim());

        // Create the intersection
        Intersection intersection = new Intersection(name, null, 0, floor);
        graph.addNode(intersection);

        // Add connected paths (hallways, stairs, elevators)
        for (int i = 2; i < parts.length; i++) {
            String connectedPath = parts[i].trim();
            Node connectedNode = graph.getNode(connectedPath);
            if (connectedNode != null) {
                intersection.addConnectedNode(connectedNode);
            }
        }
    }











    private Hallway findHallwayByName(String name) {
        for (Hallway hallway : graph.getHallways()) {
            if (hallway.name.equals(name)) {
                return hallway;
            }
        }
        return null;
    }






    
}
