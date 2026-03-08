import pandas as pd
from pathlib import PureWindowsPath, PurePosixPath, Path

data_05 = './birdnet05.csv'
data_09 = './birdnet09.csv'

def gen_paths(df1, df2):
    df = pd.concat([df1, df2]).drop_duplicates().reset_index(drop=True)
    return df['gs_FilePath'].to_list()

def win_to_posix(win_path):
    root = 'dclde_2026_killer_whales/'
    path = PureWindowsPath(win_path)
    filename = path.name
    subdirs = [p.lower() for p in path.parts[2:-1]]
    return str(PurePosixPath(root)/ Path(*subdirs) / filename)

def posix_to_uri(posix_path):
    noaa_root = 'gs://noaa-passive-bioacoustic/dclde/2027'
    return noaa_root+'/'+posix_path

def add_cols_df(path_to_df):
    df = pd.read_csv(path_to_df)
    df.rename(columns={'FilePath':'win_FilePath'}, inplace=True)
    df['pos_FilePath'] = df['win_FilePath'].apply(win_to_posix)
    df['gs_FilePath'] = df['pos_FilePath'].apply(posix_to_uri)
    return df

def main():
    df1 = add_cols_df(data_05)
    df2 = add_cols_df(data_09)
    with open('./gs_paths.txt', "a") as f:
        for p in gen_paths(df1, df2):
            f.write(p+'\n')
    df1.to_csv('./local_birdnet05.csv')
    df2.to_csv('./local_birdnet09.csv')

if __name__ == '__main__':
    main()