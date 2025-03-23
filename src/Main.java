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
        int timeConstraint = Integer.parseInt(args[2]); // Time constraint in minutes 
        boolean mobilityConstraints = Boolean.parseBoolean(args[3]); // Mobility constraints (true/false)

        Graph graph = new Graph();
        Parser parser = new Parser(graph);
        parser.initializeBuildingMap("../data/bissetBuilding.txt");
        CRF crf = new CRF(graph);
        List<Node> route = crf.findRoute(startRoom, endRoom, timeConstraint, mobilityConstraints);
        crf.displayRoute(route);
    }
}