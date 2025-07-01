from docx import Document
import difflib

def get_lines(docx_path):
    doc = Document(docx_path)
    return [para.text for para in doc.paragraphs if para.text.strip()]

lines1 = get_lines(r"A:\Personal Files\Education\North Carolina State University\ST542\ST542_ConsultProj\DocComps\EMPLOYERS_OLD.docx")
lines2 = get_lines(r"A:\Personal Files\Education\North Carolina State University\ST542\ST542_ConsultProj\DocComps\EMPLOYERS_NEW.docx")

print(f"Lines in OLD: {len(lines1)}")
print(f"Lines in NEW: {len(lines2)}")


diff = difflib.unified_diff(lines1, lines2, lineterm="", fromfile="file1", tofile="file2")

for line in diff:
    print(line)

