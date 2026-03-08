import pandas as pd
from pathlib import PureWindowsPath
from collections import defaultdict


data_05 = './birdnet05.csv'
data_09 = './birdnet09.csv'
roots = set()
datasets = set()
subdirs = defaultdict(set)

def gen_paths(df1, df2):
    df1 = pd.read_csv(df1)
    df2 = pd.read_csv(df2)
    df = pd.concat([df1, df2]).drop_duplicates().reset_index(drop=True)
    return df['FilePath'].to_list()

def parse_dir(path):
    path = PureWindowsPath(path)
    root = path.parts[1]
    dataset = path.parts[2]
    subs = path.parts[3:-1]
    roots.add(root)
    datasets.add(dataset)
    subdirs[dataset].add("/".join(subs))

def main():
    all_paths = gen_paths(data_05, data_09)
    for p in all_paths:
        parse_dir(p)

if __name__ == "__main__":
    main()
    


