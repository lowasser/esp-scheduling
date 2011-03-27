import java.util.Set;

public class Class {
	int cId;
	int duration;
	Section[] sectionTimes;
	Section[] sections;
	Set<Teacher> teachers;
	Set<Room> compatibleRooms;

	public Class(int cId, int duration, int numSections, int numBlocks,
			Set<Teacher> teachers, Set<Room> compatibleRooms) {
		this.cId = cId;
		this.duration = duration;
		sectionTimes = new Section[numBlocks];
		sections = new Section[numSections];
		this.teachers = teachers;
		this.compatibleRooms = compatibleRooms;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + cId;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Class))
			return false;
		Class other = (Class) obj;
		if (cId != other.cId)
			return false;
		return true;
	}

}
