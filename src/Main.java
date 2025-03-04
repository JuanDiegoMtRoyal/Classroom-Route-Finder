import java.util.List;

public class Main {
     public static void main(String[] args) {
        if (args.length == 1 && args[0].equals("test")){
            runTests();
            return;
        }
        if (args.length < 4) {
            System.out.println("Usage: java Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>");
            System.out.println("Example: java Main EB1113 EB1112 2 false");
            return;
        }

        String startRoom = args[0]; // Starting classroom
        String endRoom = args[1];   // Destination classroom
        int timeConstraint = Integer.parseInt(args[2]); // Time constraint in minutes /add catch block incase they dont enter a number
        boolean mobilityConstraints = Boolean.parseBoolean(args[3]); // Mobility constraints (true/false)

        mainLogic(startRoom, endRoom, timeConstraint, mobilityConstraints);
    }

    private static void mainLogic(String startRoom, String endRoom, int time, boolean mobility){
        // Create the graph
        Graph graph = new Graph();

        // Parse the building map
        Parser parser = new Parser(graph);
        parser.initializeBuildingMap("../data/bissetFloor1.txt");

        // Create the CRF (Classroom Route Finder)
        CRF crf = new CRF(graph);

        
        // Find a route based on user input
        List<Node> route = crf.findRoute(startRoom, endRoom, time, mobility);

        // Display the route
        crf.displayRoute(route);
    }

    private static void runTests(){
        System.out.println("\nRunning detailed test cases for Classroom Route Finder...");
        
        Graph graph = new Graph();
        System.out.println("1.) Initializing Graph...\n        => " + graph.getAllNodes());

        Parser parser = new Parser(graph);
        System.out.println("2a.) Parsing Building Map...");
        parser.initializeBuildingMap("bissetFloor1.txt");
        System.out.println("2b.) Parsing Building Map...");
        parser.initializeBuildingMap("../data/bissetFloor1.txt");

        CRF crf = new CRF(graph);
        System.out.println("3.) Creating Classroom Route Finder...\n    Graph consists of:");
        for (Node node : graph.getAllNodes()) {
            System.out.println("        => " + node.name);
        }

        Object[][] testCases = {
            {"EB1023", "EB1028", 5, false},     // Standard case (valid route)
            {"EB1109", "EB1113", 5, true},      // mobility = true
            {"EB1023", "EB1028", 0, false},     // Time constraint too strict
            {"EB9999", "EB1112", 5, false},     // Nonexistent classroom
            {"EB1113", "EB1113", 5, false},     // Start and end are the same
            {"EB1010", "EB1113", 2, false}      // Failed test case
        };

        for (Object[] test : testCases) {
            System.out.println("--------------------------------------------------");
            System.out.printf("Test Case: %s -> %s | Time: %d min | Mobility: %s%n",
                    test[0], test[1], test[2], test[3]);

            System.out.println("Finding route for test cases...");
            List<Node> route = crf.findRoute((String) test[0], (String) test[1], (int) test[2], (boolean) test[3]);
            crf.displayRoute(route);
        }
    }
}