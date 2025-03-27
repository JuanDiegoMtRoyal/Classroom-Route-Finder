import java.util.List;

public class Main {
    public static void main(String[] args) {
        try {
            if (args.length < 4) {
                System.out.println("Usage: java Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>");
                System.out.println("Example: java Main EB1113 EB1112 2 false");
                return;
            }

            String startRoom = args[0].toUpperCase(); // Starting classroom
            String endRoom = args[1].toUpperCase(); // Destination classroom
            int timeConstraint;
            try {
                timeConstraint = Integer.parseInt(args[2]); // Time constraint in minutes
                if (timeConstraint <= 0 || timeConstraint > 6) {
                    throw new IllegalArgumentException("Time constraint must be between 1 and 6 minuets");
                }
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Time constraint must be a valid integer");
            }
            boolean mobilityConstraints; //Mobility constraint (true/false) *true disallows stairs and false disallows elevators
            if (args[3].equalsIgnoreCase("true") || args[3].equalsIgnoreCase("false")) {
                mobilityConstraints = Boolean.parseBoolean(args[3]);
            } else {
                throw new IllegalArgumentException("Mobility constraints must be 'true' or 'false'");
            }

            System.out.println(
                    "\nNavigating from Classroom " + startRoom + " to Classroom " + endRoom
                            + " with a time constraint of "
                            + timeConstraint + " minutes and mobility constraint: " + mobilityConstraints);

            Graph graph = new Graph(); //graph that stores all nodes

            Parser parser = new Parser(graph);
            parser.initializeBuildingMap("../data/bissetBuilding.txt");
            parser.resolveAllConnections(); //resolves connections from objects to objects that did not exist at time of establishing connection

            CRF crf = new CRF(graph);//solution finder

            List<Node> route = crf.findRoute(startRoom, endRoom, timeConstraint, mobilityConstraints);
            crf.displayRoute(route);

        } catch (IllegalArgumentException e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("An unexpected error occurred: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);

        }
    }
}