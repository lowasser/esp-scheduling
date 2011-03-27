public class Teacher {
	int teacherId;
	Section[] myClasses;

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + teacherId;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Teacher))
			return false;
		Teacher other = (Teacher) obj;
		if (teacherId != other.teacherId)
			return false;
		return true;
	}

	public Teacher(int teacherId, int numBlocks) {
		this.teacherId = teacherId;
		this.myClasses = new Section[numBlocks];
	}
}
