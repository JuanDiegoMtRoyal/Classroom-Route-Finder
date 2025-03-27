import java.util.*;

abstract class Node {
    String name;
    int floor;
    Intersection intersection;
    Hallway hallway;
    int positionAlongHallway;

    public Node(String name, Intersection intersection, Hallway hallway, int positionAlongHallway, int floor) {
        this.name = name;
        this.intersection = intersection;
        this.hallway = hallway;
        this.positionAlongHallway = positionAlongHallway;
        this.floor = floor;
    }

    public abstract void displayInfo();

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;
        Node node = (Node) obj;
        return Objects.equals(name, node.name);
    }

}
