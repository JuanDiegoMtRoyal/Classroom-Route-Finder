public class Hallway {
    String name;
    String building;
    String direction1;
    String direction2;
    int floor;
    int length;

    public Hallway(String name, String building, String startIntersection, String direction1, String direction2, int floor, int length) {
        this.name = name;
        this.building = building;
        this.startIntersection = startIntersection;
        this.direction1 = direction1;
        this.direction2 = direction2;
        this.floor = floor;
        this.length = length;
    }


    
}
