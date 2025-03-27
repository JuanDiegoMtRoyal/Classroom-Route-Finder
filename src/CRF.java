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




    private boolean dfs(Node current, Node end, int remainingTime, boolean mobilityConstraints, Set<Node> visited,
            List<Node> route) {

        visited.add(current);
        route.add(current);
        if (current.equals(end))
            return true;

        List<Node> neighbors = new ArrayList<>();

        // 1. Nodes in current hallway: Add adjacent nodes
        if (current.hallway != null && !(current instanceof Intersection) 
        && !(current instanceof Stairs) && !(current instanceof Elevator)) {
    List<Node> hallwayNodes = current.hallway.getNodes();
    int currentIndex = hallwayNodes.indexOf(current);
    
    if (currentIndex != -1) {
        // Add previous node
        if (currentIndex > 0) {
            Node prevNode = hallwayNodes.get(currentIndex - 1);
            neighbors.add(prevNode);
        }
        // Add next node
        if (currentIndex < hallwayNodes.size() - 1) {
            Node nextNode = hallwayNodes.get(currentIndex + 1);
            neighbors.add(nextNode);
        }
    }
}


    if (current instanceof Intersection) {
        for (Node node : ((Intersection) current).getConnectedNodes()) {
            // Same floor: exclude stairs/elevators
            if (current.floor == end.floor){
                if(!(node instanceof Stairs || node instanceof Elevator))
                {
                    neighbors.add(node);
                }
            }

            else{
                if ((mobilityConstraints && node instanceof Elevator) || (!mobilityConstraints && node instanceof Stairs)) {
                    neighbors.add(node);
            }
            }
        }
    
            }


          if (current instanceof Stairs || current instanceof Elevator) {
        
        // Same floor: exit to connected intersection
        if (current.floor == end.floor) {
            if (current.intersection != null) {
                neighbors.add(current.intersection);
            }
        }
        // Different floor: handle connections
        else {
            List<Node> transportNodes = (current instanceof Stairs) 
                ? ((Stairs) current).getConnectedNodes() 
                : ((Elevator) current).getConnectedNodes();
            
            for (Node node : transportNodes) {
                if ((node instanceof Stairs || node instanceof Elevator) && 
                    node.floor != current.floor) {
                    neighbors.add(node);
                }
            }
        }
    }

    
 // 4. Explore neighbors
 for (Node neighbor : neighbors) {
    if (!visited.contains(neighbor)) {
        int timeCost = calculateTimeCost(current, neighbor);
        
        if (remainingTime - timeCost >= 0) {

            if (dfs(neighbor, end, remainingTime - timeCost, mobilityConstraints, visited, route)) {
                return true;
            }
        } 
    }
}

// Backtrack
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

             if((from instanceof Stairs || from instanceof Elevator || from instanceof Intersection) && 
             (to instanceof Stairs|| to instanceof Elevator || to instanceof Intersection) &&
             from.floor == to.floor)
             {
                return 0; 
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
    
        System.out.println("\n\t\tRoute Directions:\n");
        int totalTimeSeconds = 0;
    
        for (int i = 0; i < route.size(); i++) {
            Node current = route.get(i);
            current.displayInfo();
    
            if (i < route.size() - 1) {
                Node next = route.get(i + 1);
                totalTimeSeconds += calculateTimeCost(current, next);
            }
        }
    
        int minutes = totalTimeSeconds / 60;
        int seconds = totalTimeSeconds % 60;
        System.out.println("\nYou have arrived. Total time taken: " + minutes + " minutes and " + seconds + " seconds.");
    }
}
