import java.io.*;
import java.util.ArrayList;
import java.util.List;


public class Parser {
    private Graph graph;
    public Parser(Graph graph) {
        this.graph = graph;
    }

/**
 * @brief 
 *
 * Insert indepth 
 *
 * @param 
 */    
public void initializeBuildingMap(String filename)
    {
        try(BufferedReader reader = new BufferedReader(new FileReader(filename)))
        {
            String line;
            String currentSection = null;
            while((line = reader.readLine()) != null)
            {
                line = line.trim();

                System.out.println("parsing line: " + line);
                if(line.isEmpty())
                {
                    continue; 
                }

                if(line.startsWith("#"))
                {
                    System.out.println("line starts with #");
                if(line.contains("Intersection Format"))
                {
                    System.out.println("current section is \t Intersection");
                    currentSection = "Intersection";
                }
                  else if(line.contains("Hallway Format"))
                  {
                    System.out.println("current section is \t Hallway");
                    currentSection = "Hallway";
                  }  
                  else if(line.contains("Classroom Format"))
                  {
                    System.out.println("current section is \t Classroom");
                    currentSection = "Classroom";
                  }
                  else if(line.contains("Stairs Format"))
                  {
                    System.out.println("current section is \t Istairs");
                    currentSection = "Stairs";
                  }
                  else if(line.contains("Elevator Format"))
                  {
                    System.out.println("current section is \t Elevatorn");
                    currentSection = "Elevator";
                  }
                  continue;
                }

                switch(currentSection)
                {
                    case "Hallway":
                    parseHallway(line);
                    System.out.println("\n\n\n");
                    break;
                    case "Classroom":
                    parseClassroom(line);
                    System.out.println("\n\n\n");
                    break;
                case "Stairs":
                    parseStairs(line);
                    System.out.println("\n\n\n");
                    break;
                case "Elevator":
                    parseElevator(line);
                    System.out.println("\n\n\n");
                    break;
                case "Intersection":
                    parseIntersection(line);
                    System.out.println("\n\n\n");
                    break;
                default:
                System.out.println("unknown section skipping line:" + line);
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

    /**
 * @brief Parses the parameters of a hallway in input txt file
 *
 * Parses the parametes of the hallway and checks if the start intersection object exists
 * if not it creates it then adds the start intersection to the graph object.
 * It then creates the hallway object and adds it to the start intersection object and 
 * vise versa is added to the hallwy object so they are linked. Finally the hallway object is
 * added to the graph object. 
 *
 * @param String containing information about the hallway
 */
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
        if (startIntersection == null) {
            System.out.println("Creating new start intersection with name: " + startIntersectionName);
            startIntersection = new Intersection(startIntersectionName, null, 0, floor);
            graph.addNode(startIntersection);
            System.out.println("StartIntersection: " + startIntersectionName + " added to graph");
        }
        Hallway hallway = new Hallway(name, building, startIntersection, direction1, direction2, floor, length);
        System.out.println("created hallway: " + hallway.name);

        startIntersection.addHallway(hallway);
        System.out.println("hallway: " + hallway.name + " added to startintersection: " + startIntersectionName);  
        hallway.addNode(startIntersection);
        System.out.println("startintersection: " + startIntersectionName + " added to hallway: " + hallway.name);
        graph.addHallway(hallway);
        System.out.println("hallway: " + hallway.name + " added to graph");
    }

/**
 * @brief Parses the parameters of a classroom in input txt file
 *
 * Parses the parameters of a classroom and creates a Classroom object.
 * It then finds the hallway that the classroom belongs to and adds it to 
 * the hallway objects list of classrooms. The classroom is then added to 
 * the list of nodes in the graph object. 
 *
 * @param line String containing information about the classroom
 */
    private void parseClassroom(String line) {

        System.out.println("parsing classroom with line: " + line);
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
        System.out.println("looking for hallway for: " + line);
        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Classroom classroom = new Classroom(name, building, hallway, position, compassDirection, floor);
            graph.addNode(classroom);
            System.out.println("classroom" + classroom.name + " added to graph");
            hallway.addNode(classroom);
            System.out.println("classroom" + classroom.name + " added to hallway" + hallway.name);
        }

        else{
            System.out.println("ERROR FINDING HALLWAY/ADDING CLASSROOM: " + line );
        }
    }

/**
 * @brief 
 *
 * Insert indepth 
 *
 * @param 
 */
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
        int floor = Integer.parseInt(parts[parts.length - 1].trim());

        // Find the hallway
        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Stairs stairs = new Stairs(name, hallway, position, floor);

            for (int i = 5; i < parts.length - 1; i++) {
                stairs.addConnectedNodeName(parts[i].trim());
            }
            graph.addNode(stairs);
            hallway.addNode(stairs);

        }
    }

/**
 * @brief 
 *
 * Insert indepth 
 *
 * @param 
 */
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

        /*
        String teleportTo = parts[5].trim();
        int floor = Integer.parseInt(parts[6].trim());
        */

        int floorIndex = parts.length - 1;
        int floor = Integer.parseInt(parts[floorIndex].trim());

        // Find the hallway
        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Elevator elevator = new Elevator(name, hallway, position, floor);
            graph.addNode(elevator);
            hallway.addNode(elevator);
        }
    }



/**
 * @brief 
 *
 * Insert indepth 
 *
 * @param 
 */
    private void parseIntersection(String line) {

        System.out.println("parsing intersection line: " + line);
        String[] parts = line.split("[(),]");
       
        String name = parts[0].trim();
        int floor = Integer.parseInt(parts[1].trim());
        String hallwayName =  parts[2].trim();
        int posOnHallway = Integer.parseInt(parts[3].trim());

        System.out.println("intersection info: name:"+name+" floor"+floor+" onhallway:"+hallwayName+" with position:"+posOnHallway);


        Intersection intersection = (Intersection) graph.getNode(name);
        if(intersection == null){
            System.out.println("intersection: " + name + " did not exist so creating");
            intersection = new Intersection(name, null, posOnHallway, floor); //for now exclude the hallway its on
            graph.addNode(intersection);
        }

        for(int i = 4; i < parts.length; i++){
            String item = parts[i].trim();

            if(item.startsWith("hallway")){
                System.out.println("adding a hallway connected path to intersection: " + intersection.name);
                Hallway hallway = findHallwayByName(item);
                if(hallway !=null){
                    intersection.addHallway(hallway);
                    System.out.println("intersection: " + intersection.name + "has added hallway: " + hallway.name);
                    if(!hallway.getNodes().contains(intersection)){
                        hallway.addNode(intersection);
                        System.out.println("hallway: " + hallway.name + " has added node intersection: " + intersection.name);
                    }
                }
            }

            else{
                System.out.println("adding a normal node connected path to intersection: " + intersection.name);
                System.out.println("attempting to add: " + item);
                Node node = graph.getNode(item);
                if(node != null){
                    intersection.addConnectedNode(node);
                    System.out.println("added a: " + node.name + " to intersection: " + intersection.name);
                }
            }
        }
        for (Hallway hallway : intersection.getConnectedHallways()) {
        List<Node> hallwayNodes = hallway.getNodes();
        if (!hallwayNodes.isEmpty()) {
            // Connect to start and end of hallway
            intersection.addConnectedNode(hallwayNodes.get(0));
            intersection.addConnectedNode(hallwayNodes.get(hallwayNodes.size()-1));
        }
    }
    }


/**
 * @brief returns an object of type Hallway matching the name specified
 *
 * iterates through the hallway objects in the graph object and once 
 * the names match it returns that hallway.
 *
 * @param String of the hallway name
 */
    private Hallway findHallwayByName(String name) {
        for (Hallway hallway : graph.getHallways()) {
            if (hallway.name.equals(name)) {
                System.out.println("found hallway " + hallway.name + " that equals " + name);
                return hallway;
            }
        }
        System.out.println("hallway with name: " + name + " NOT FOUND");
        return null;
        
    }

    public void resolveAllConnections() {
        for (Node node : graph.getAllNodes()) {
            if (node instanceof Stairs) {
                ((Stairs) node).resolveConnections(graph);
            }
            if (node instanceof Elevator) {
                ((Elevator) node).resolveConnections(graph);
            }
        }
    }

    
    
}
