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
     *        Insert indepth
     *
     * @param
     */
    public void initializeBuildingMap(String filename) {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            String currentSection = null;
            while ((line = reader.readLine()) != null) {
                line = line.trim();

                if (line.isEmpty()) {
                    continue;
                }

                if (line.startsWith("#")) {
                    if (line.contains("Intersection Format")) {
                        currentSection = "Intersection";
                    } else if (line.contains("Hallway Format")) {
                        currentSection = "Hallway";
                    } else if (line.contains("Classroom Format")) {
                        currentSection = "Classroom";
                    } else if (line.contains("Stairs Format")) {
                        currentSection = "Stairs";
                    } else if (line.contains("Elevator Format")) {
                        currentSection = "Elevator";
                    }
                    continue;
                }

                switch (currentSection) {
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

        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    /**
     * @brief Parses the parameters of a hallway in input txt file
     *
     *        Parses the parametes of the hallway and checks if the start
     *        intersection object exists
     *        if not it creates it then adds the start intersection to the graph
     *        object.
     *        It then creates the hallway object and adds it to the start
     *        intersection object and
     *        vise versa is added to the hallwy object so they are linked. Finally
     *        the hallway object is
     *        added to the graph object.
     *
     * @param String containing information about the hallway
     */
    private void parseHallway(String line) {
        String[] parts = line.split("[(),]");
        if (parts.length < 7) {
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

        Intersection startIntersection = (Intersection) graph.getNode(startIntersectionName);
        if (startIntersection == null) {
            startIntersection = new Intersection(startIntersectionName, 0, floor);
            graph.addNode(startIntersection);
        }
        Hallway hallway = new Hallway(name, building, startIntersection, direction1, direction2, floor, length);

        startIntersection.addHallway(hallway);
        hallway.addNode(startIntersection);
        graph.addHallway(hallway);
    }

    /**
     * @brief Parses the parameters of a classroom in input txt file
     *
     *        Parses the parameters of a classroom and creates a Classroom object.
     *        It then finds the hallway that the classroom belongs to and adds it to
     *        the hallway objects list of classrooms. The classroom is then added to
     *        the list of nodes in the graph object.
     *
     * @param line String containing information about the classroom
     */
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

        Hallway hallway = findHallwayByName(hallwayName);
        if (hallway != null) {
            Classroom classroom = new Classroom(name, building, hallway, position, compassDirection, floor);
            graph.addNode(classroom);
            hallway.addNode(classroom);
        }

        else {
            System.out.println("ERROR FINDING HALLWAY/ADDING CLASSROOM: " + line);
        }
    }

    /**
     * @brief
     *
     *        Insert indepth
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
        String intersectionName = parts[2].trim();
        int position = Integer.parseInt(parts[3].trim());
        String compassDirection = parts[4].trim();
        int floor = Integer.parseInt(parts[parts.length - 1].trim());

        Stairs stairs = new Stairs(name, position, floor);
        stairs.addConnectedNodeName(intersectionName);

        for (int i = 5; i < parts.length - 1; i++) {
            stairs.addConnectedNodeName(parts[i].trim()); // add teleportation nodes names
        }
        graph.addNode(stairs);
    }

    /**
     * @brief
     *
     *        Insert indepth
     *
     * @param
     */
    private void parseElevator(String line) {
        String[] parts = line.split("[(),]");
        if (parts.length < 7) {
            System.err.println("Invalid stairs format: " + line);
            return;
        }

        String name = parts[0].trim();
        String building = parts[1].trim();
        String intersectionName = parts[2].trim();
        int position = Integer.parseInt(parts[3].trim());
        String compassDirection = parts[4].trim();
        int floor = Integer.parseInt(parts[parts.length - 1].trim());

        Elevator elevator = new Elevator(name, position, floor);
        elevator.addConnectedNodeName(intersectionName); // link elevator to intersection

        for (int i = 5; i < parts.length - 1; i++) {
            elevator.addConnectedNodeName(parts[i].trim()); // add teleportation nodes names
        }
        graph.addNode(elevator);
    }

    private void parseIntersection(String line) {
        String[] parts = line.split("[(),]");

        String name = parts[0].trim();
        int floor = Integer.parseInt(parts[1].trim());
        int posOnHallway = Integer.parseInt(parts[2].trim());

        // Find or create intersection
        Intersection intersection = (Intersection) graph.getNode(name);
        if (intersection == null) {
            intersection = new Intersection(name, posOnHallway, floor);
            graph.addNode(intersection);
        }

        // Process connected items
        for (int i = 3; i < parts.length; i++) {
            String item = parts[i].trim();

            if (item.startsWith("hallway")) {
                Hallway hallway = findHallwayByName(item);
                if (hallway != null) {
                    intersection.addHallway(hallway);

                    // Add intersection to hallway if not already present
                    if (!hallway.getNodes().contains(intersection)) {
                        hallway.addNode(intersection);
                        hallway.addIntersection(intersection);
                    }
                }
            } else {
                Node node = graph.getNode(item);
                if (node != null) {
                    intersection.addConnectedNode(node);
                    node.intersection = intersection;
                } else {
                    System.out.println("node was null: " + item);
                }
            }
        }

        // Connect to adjacent nodes in all hallways
        for (Hallway hallway : intersection.getConnectedHallways()) {
            List<Node> hallwayNodes = hallway.getNodes();
            int intersectionIndex = hallwayNodes.indexOf(intersection);

            if (intersectionIndex == -1) {
                System.err.println("Intersection " + intersection.name + " not found in hallway " + hallway.name);
                continue;
            }

            // Connect to previous node
            if (intersectionIndex > 0) {
                Node prevNode = hallwayNodes.get(intersectionIndex - 1);
                if ((intersectionIndex - 1) >= 0) {
                    if (!prevNode.equals(intersection)) {
                        intersection.addConnectedNode(prevNode);
                    }
                }
            }

            // Connect to next node
            if (intersectionIndex < hallwayNodes.size() - 1) {
                Node nextNode = hallwayNodes.get(intersectionIndex + 1);
                if (!nextNode.equals(intersection)) {
                    intersection.addConnectedNode(nextNode);
                }
            }
        }
    }

    /**
     * @brief returns an object of type Hallway matching the name specified
     *
     *        iterates through the hallway objects in the graph object and once
     *        the names match it returns that hallway.
     *
     * @param String of the hallway name
     */
    private Hallway findHallwayByName(String name) {
        for (Hallway hallway : graph.getHallways()) {
            if (hallway.name.equals(name)) {
                return hallway;
            }
        }
        return null;

    }

    private Intersection findIntersectionByName(String name) {
        for (Intersection intersection : graph.getIntersections()) {

            if (intersection.name.equals(name)) {
                return intersection;
            }
        }
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
