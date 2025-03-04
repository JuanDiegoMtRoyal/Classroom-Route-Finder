import java.util.*;

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

        // Perform DFS with backtracking
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

    private boolean dfs(Node current, Node end, int remainingTime, boolean mobilityConstraints, Set<Node> visited, List<Node> route) {
        visited.add(current);
        route.add(current);
        if (current.equals(end)) {
            return true;
        }

        // Traverses hallway and its associated nodes
        for (Node neighbor : current.hallway.getNodes()) {
            int timeCost = calculateTimeCost(current, neighbor);
            if (!visited.contains(neighbor)) {
                if (remainingTime - timeCost >= 0) {
                    if (mobilityConstraints && neighbor instanceof Stairs) {
                        // Check if there is an elevator alternative
                        continue;
                    }
                    if (dfs(neighbor, end, remainingTime - timeCost, mobilityConstraints, visited, route)) {
                        return true;
                    }
                    remainingTime += timeCost;
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
        // Calculate the time cost based on the distance between nodes
        return Math.abs(to.positionAlongHallway - from.positionAlongHallway); //need to decide on time constrain maybe 1.4m = 1 sec?
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
    }

}
