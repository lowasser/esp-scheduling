public class Room {
	int roomNumber;
	Section[] roomAssignments;
	int x;
	int y;
	int z;

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + roomNumber;
		return result;
	}

	public Room(int roomNumber, int x, int y, int z, int numBlocks) {
		this.roomNumber = roomNumber;
		this.roomAssignments = new Section[numBlocks];
		this.x = x;
		this.y = y;
		this.z = z;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Room))
			return false;
		Room other = (Room) obj;
		if (roomNumber != other.roomNumber)
			return false;
		return true;
	}

	public int distance(Room r) {
		if (roomNumber == r.roomNumber)
			return 0;
		return 1;
		// return Math.abs(x - r.x + y - r.y + 10 * (z - r.z));
	}
}
