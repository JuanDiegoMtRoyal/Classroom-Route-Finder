import java.util.*;
import java.util.stream.Collectors;

public class CRF {
    private Graph graph;

    public CRF(Graph graph) {
        this.graph = graph;
    }


    /**
     * Finds a route from the starting classroom to the destination classroom.
     *
     * @param startRoom          The starting classroom.
     * @param endRoom            The destination classroom.
     * @param timeConstraint     The maximum time allowed for the route (in minutes).
     * @param mobilityConstraints Whether mobility constraints (elevators only/no stairs) are enabled.
     * @return A list of nodes representing the route, or null if no route is found.
     */
    public List<Node> findRoute(String startRoom, String endRoom, int timeConstraint, boolean mobilityConstraints) {
        Node startNode = graph.getNode(startRoom);
        Node endNode = graph.getNode(endRoom);

        if (startNode == null || endNode == null) {
            System.out.println("Classroom does not exist.");
            return null;
        }

        if (timeConstraint < 1) {
            System.out.println("Time constraint must be at least 1 minute.");
            return null;
        }

        List<Node> route = new ArrayList<>();
        Set<Node> visited = new HashSet<>();
        boolean found = dfs(startNode, endNode, timeConstraint * 60, mobilityConstraints, visited, route);

        if (found) {
            return route;
        } else {
            System.out.println("No accessible route available.");
            return null;
        }
    }

   
    /* takes forever to execute, unoptimized*/

    private boolean dfs(Node current, Node end, int remainingTime, boolean mobilityConstraints, Set<Node> visited,
            List<Node> route) {

        visited.add(current);
        route.add(current);
        if (current.equals(end))
            return true;

        // Get all possible neighbors
        List<Node> neighbors = new ArrayList<>();

        // 1. Nodes in current hallway
        if (current.hallway != null) {
            neighbors.addAll(current.hallway.getNodes());
        }

        // 2. Connected nodes if current is intersection
        if (current instanceof Intersection) {
            neighbors.addAll(((Intersection) current).getConnectedNodes());
        }

        if (current instanceof Stairs) {
            neighbors.addAll(((Stairs) current).getConnectedNodes());
        } else if (current instanceof Elevator) {
            neighbors.addAll(((Elevator) current).getConnectedNodes());
        }

        for (Node neighbor : neighbors) {
            if (!visited.contains(neighbor)) {
                int timeCost = calculateTimeCost(current, neighbor);

                if (remainingTime - timeCost >= 0) {
                    if (mobilityConstraints) {
                        if (neighbor instanceof Stairs || current instanceof Stairs) {
                            continue;
                        }
                    }
                }

                if (dfs(neighbor, end, remainingTime - timeCost, mobilityConstraints, visited, route)) {
                    return true;
                }
            }

        }
        // back track
        route.remove(route.size() - 1);
        visited.remove(current);
        return false;

    }

    

     /**
     * Calculates the time cost to move from one node to another.
     *
     * @param from The starting node.
     * @param to   The destination node.
     * @return The time cost (in seconds).
     */

    
    private int calculateTimeCost(Node from, Node to) {
        if((from instanceof Stairs || from instanceof Elevator) && 
             (to instanceof Stairs || to instanceof Elevator) &&
             from.floor != to.floor)
             {
                return 30; 
             }
        // Calculate the time cost based on the distance between nodes
        return Math.abs(to.positionAlongHallway - from.positionAlongHallway);
    }
    
    /**
     * Displays the route directions.
     *
     * @param route The route to display.
     */
    public void displayRoute(List<Node> route) {
        if (route == null || route.isEmpty()) {
            System.out.println("No route found.");
            return;
        }

        System.out.println("Route Directions:");
        for (Node node : route) {
            node.displayInfo();
        }
        System.out.println("\nYou have arrived, Thanks for navigating with us");

    }





    //ai stuff
public void printFullGraph() {
    System.out.println("\n=== FULL GRAPH STRUCTURE ===");
    
    // Group nodes by floor
    Map<Integer, List<Node>> nodesByFloor = graph.getAllNodes().stream()
        .collect(Collectors.groupingBy(n -> n.floor));

    nodesByFloor.forEach((floor, nodes) -> {
        System.out.println("\nFloor " + floor + ":");
        
        nodes.forEach(node -> {
            System.out.println("\n" + node.name + " (" + node.getClass().getSimpleName() + ")");
            System.out.println("  Hallway: " + (node.hallway != null ? node.hallway.name : "N/A"));
            System.out.println("  Position: " + node.positionAlongHallway + "m");
            
            if (node instanceof Intersection) {
                Intersection i = (Intersection) node;
                System.out.println("  Connected Hallways: " + 
                    i.getConnectedHallways().stream()
                        .map(h -> h.name)
                        .collect(Collectors.joining(", ")));
                System.out.println("  Connected Nodes: " + 
                    i.getConnectedNodes().stream()
                        .map(n -> n.name)
                        .collect(Collectors.joining(", ")));
            }
            
            if (node instanceof Stairs) {
                System.out.println("  Connects to: " + 
                    ((Stairs) node).getConnectedNodes().stream()
                        .map(n -> n.name + " (F" + n.floor + ")")
                        .collect(Collectors.joining(", ")));
            }
            
            if (node instanceof Elevator) {
                System.out.println("  Connects to: " + 
                    ((Elevator) node).getConnectedNodes().stream()
                        .map(n -> n.name + " (F" + n.floor + ")")
                        .collect(Collectors.joining(", ")));
            }
        });
    });
}

}
