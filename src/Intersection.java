import java.util.ArrayList;
import java.util.List;

public class Intersection extends Node{
    private List<Node> connectedNodes;


    public Intersection(String name, Hallway hallway, int positionAlongHallway, int floor) {
        super(name, hallway, positionAlongHallway, floor);
        this.connectedNodes = new ArrayList<>();
    }

    public void addConnectedNode(Node node) {
        connectedNodes.add(node);
    }

    public List<Node> getConnectedNodes() {
        return connectedNodes;
    }


    @Override
    public void displayInfo() {
        System.out.println("Intersection: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
        System.out.println("Connected Nodes:");
        for (Node node : connectedNodes) {
            System.out.println("  - " + node.name);
        }
    }   
    
}
