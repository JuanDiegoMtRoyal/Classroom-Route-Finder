import java.util.List;

public class Main {
    public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println("Usage: java Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>");
            System.out.println("Example: java Main EB1113 EB1112 2 false");
            return;
        }

        String startRoom = args[0].toUpperCase(); // Starting classroom
        String endRoom = args[1].toUpperCase(); // Destination classroom
        int timeConstraint = Integer.parseInt(args[2]); // Time constraint in minutes
        boolean mobilityConstraints = Boolean.parseBoolean(args[3].toUpperCase()); // Mobility constraints (true/false)

        System.out.println(
                "\nNavigating from Classroom " + startRoom + " to Classroom " + endRoom + " with a time constraint of "
                        + timeConstraint + " minutes and mobility constraint: " + mobilityConstraints);

        Graph graph = new Graph();
        Parser parser = new Parser(graph);
        parser.initializeBuildingMap("../data/bissetBuilding.txt");
        parser.resolveAllConnections();
        CRF crf = new CRF(graph);

        List<Node> route = crf.findRoute(startRoom, endRoom, timeConstraint, mobilityConstraints);
   
        crf.displayRoute(route);

    }
}