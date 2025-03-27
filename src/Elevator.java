import java.util.ArrayList;
import java.util.List;

public class Elevator extends Node {
    private List<Node> connectedNodes;
    private List<String> connectedNodeNames;

    public Elevator(String name, int positionAlongHallway, int floor) {
        super(name, null, null, positionAlongHallway, floor);
        this.connectedNodes = new ArrayList<>();
        this.connectedNodeNames = new ArrayList<>();
    }

    // adds connection name when connection object does not exist yet
    public void addConnectedNodeName(String nodeName) {
        connectedNodeNames.add(nodeName);
    }

    // uses the names of connection and all the nodes in the graph to reolve the
    // name connection to actual objects
    public void resolveConnections(Graph graph) {
        for (String name : connectedNodeNames) {
            Node node = graph.getNode(name);
            if (node instanceof Elevator) {
                connectedNodes.add(node);
                ((Elevator) node).connectedNodes.add(this);
            } else if (node instanceof Intersection) {
                connectedNodes.add(node);
                ((Intersection) node).addConnectedNode(this);
            }
        }
    }

    public List<Node> getConnectedNodes() {
        return connectedNodes;
    }

    @Override
    public void displayInfo() {
        System.out.println("-Take Elevator: " + name + " at intersection: " + intersection.name);
    }
}
