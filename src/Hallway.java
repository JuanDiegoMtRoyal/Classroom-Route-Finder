import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class Hallway {
    String name;
    String building;
    Intersection startIntersection; // the intersection where the hallway starts
    String direction1; // direction of the hallway
    String direction2; // opposite direction of the hallway
    int floor;
    int length; // length of hallway in meters
    List<Node> nodes; // nodes along the hallway (classroom,stairs,elevator,intersections)
    List<Intersection> intersections;

    public Hallway(String name, String building, Intersection startIntersection, String direction1, String direction2,
            int floor, int length) {
        this.name = name;
        this.building = building;
        this.startIntersection = startIntersection;
        this.direction1 = direction1;
        this.direction2 = direction2;
        this.floor = floor;
        this.length = length;
        this.nodes = new ArrayList<>();
        this.intersections = new ArrayList<>();
    }

    public void addNode(Node node) {
        nodes.add(node);
    }

    public List<Node> getNodes() {
        return nodes.stream()
                .sorted(Comparator.comparingInt(n -> n.positionAlongHallway))
                .collect(Collectors.toList());
    }

    public Intersection getStartIntersection() {
        return startIntersection;
    }

    public List<Intersection> getAllIntersections() {
        return intersections;
    }

    public void addIntersection(Intersection intersection) {
        intersections.add(intersection);
    }

    public int getLength() {
        return length;
    }

}
