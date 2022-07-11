import glob

files = glob.glob("data/*")

for file in files:
    print("Reading:", file)
    with open(file, "r") as f:
        data = f.read()
    lines = data.split("\n")
    for l in range(len(lines) - 2):
        if "{" in lines[l]:
            lines[l] = lines[l] + ","
    cleaned = "[" + "\n".join(lines) + "]"
    new_file = "cleaned/" + file.split("/")[-1] + ".json"
    print("Writing to:", new_file)
    with open(new_file, "w") as f:
        f.write(cleaned)
