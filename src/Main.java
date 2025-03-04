import java.util.List;

public class Main {
     public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println("Usage: java Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>");
            System.out.println("Example: java Main EB1113 EB1112 2 false");
            return;
        }

        String startRoom = args[0]; // Starting classroom
        String endRoom = args[1];   // Destination classroom
        int timeConstraint = Integer.parseInt(args[2]); // Time constraint in minutes /add catch block incase they dont enter a number
        boolean mobilityConstraints = Boolean.parseBoolean(args[3]); // Mobility constraints (true/false)

        // Create the graph
        Graph graph = new Graph();

        // Parse the building map
        Parser parser = new Parser(graph);
        parser.initializeBuildingMap("../data/bissetFloor1.txt");

        // Create the CRF (Classroom Route Finder)
        CRF crf = new CRF(graph);

        
        // Find a route based on user input
        List<Node> route = crf.findRoute(startRoom, endRoom, timeConstraint, mobilityConstraints);

        // Display the route
        crf.displayRoute(route);
    }
    
}
