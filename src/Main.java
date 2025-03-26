import java.util.List;

public class Main {
     public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println("Usage: java Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>");
            System.out.println("Example: java Main EB1113 EB1112 2 false");
            return;
        }

        String startRoom = args[0].toUpperCase(); // Starting classroom
        String endRoom = args[1].toUpperCase();   // Destination classroom
        int timeConstraint = Integer.parseInt(args[2]); // Time constraint in minutes 
        boolean mobilityConstraints = Boolean.parseBoolean(args[3].toUpperCase()); // Mobility constraints (true/false)

        System.out.println("user has entered SR,ER,TC,MC: " + startRoom + " " + endRoom + " " + timeConstraint + " " + mobilityConstraints);

        System.out.println("creating graph object");

        Graph graph = new Graph();
        System.out.println("creating parser object with graph: " + graph);
        Parser parser = new Parser(graph);

        System.out.println("beginning parse of txt file");
        parser.initializeBuildingMap("../data/bissetBuilding.txt");

        System.out.println("\n=== GRAPH SUMMARY ===");
System.out.println("Total nodes: " + graph.getAllNodes().size());
System.out.println("Total hallways: " + graph.getHallways().size());

        System.out.println("resolving connections...");
        parser.resolveAllConnections(); 


        System.out.println("creating crf object");
        CRF crf = new CRF(graph);


      

        System.out.println("searching for route....");
        List<Node> route = crf.findRoute(startRoom, endRoom, timeConstraint, mobilityConstraints);
        System.out.println("displaying route...");
        crf.displayRoute(route);
        crf.printFullGraph();

    }
}