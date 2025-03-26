import java.util.ArrayList;
import java.util.List;


public class Stairs extends Node {
    private List<Node> connectedNodes;
    private List<String> connectedNodeNames;

    public Stairs(String name, Hallway hallway, int positionAlongHallway, int floor) {
        super(name, hallway, positionAlongHallway, floor);
        this.connectedNodes = new ArrayList<>();
        this.connectedNodeNames = new ArrayList<>();
    }

    
    // Store names during parsing
    public void addConnectedNodeName(String nodeName) {
        connectedNodeNames.add(nodeName);
    }

        // Resolve names to nodes after parsing
        public void resolveConnections(Graph graph) {
            for (String name : connectedNodeNames) {
                Node node = graph.getNode(name);
                if (node instanceof Stairs) {
                    connectedNodes.add(node);
                    ((Stairs) node).connectedNodes.add(this);
                }
            }
        }


    public List<Node> getConnectedNodes() {
        return connectedNodes;
    }

    @Override
    public void displayInfo() {
        System.out.println(" Take Stairs: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
    }
}