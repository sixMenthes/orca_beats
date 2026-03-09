import polars as pl
import math

pl.set_random_seed(59)
df = pl.read_csv('./ds/local_birdnet05.csv')

DS_SIZE = math.ceil(df.shape[0]*0.5)
SRKW_BALANCE = math.ceil(DS_SIZE*0.05)
TKW_BALANCE = math.ceil(DS_SIZE*0.05)

df = df.filter((df['KW'] == True) & (df['isPulsedCalls'] == True))

pool_srkw_ct = df.filter((df['CallType'].is_not_null()) & (df['Ecotype'] == 'SRKW'))
pool_srkw_ct = pool_srkw_ct.filter(pl.col("CallType").str.contains(r"^S[0-9]{1,2}$", strict=True))
pool_tkw_ct = df.filter((df['CallType'].is_not_null()) & (df['Ecotype'] == 'TKW'))
other_srkw = df.filter(pl.col('Ecotype') == 'SRKW').join(pool_srkw_ct, on='', how='anti')
other_tkw = df.filter(pl.col('Ecotype')=='TKW').join(pool_tkw_ct, on='', how='anti')


srkw_ct_tokens = pool_srkw_ct['CallType'].unique().to_list()
tkw_ct_tokens = pool_tkw_ct['CallType'].unique().to_list()

srkw_balanced_rows = []
tkw_balanced_rows = []

for t in srkw_ct_tokens:
    srkw_balanced_rows.append(pool_srkw_ct.filter(pool_srkw_ct['CallType']==t).sample(SRKW_BALANCE//len(srkw_ct_tokens)))

srkw_ct_def = pl.concat([*srkw_balanced_rows])

for t in tkw_ct_tokens:
    tkw_balanced_rows.append(pool_tkw_ct.filter(pool_tkw_ct['CallType']==t).sample(TKW_BALANCE//len(tkw_ct_tokens)))

tkw_ct_def = pl.concat([*tkw_balanced_rows])

diff_srkw = (DS_SIZE//2)-srkw_ct_def.shape[0]
diff_tkw = (DS_SIZE//2)-tkw_ct_def.shape[0]

srkw_ct_def = pl.concat([srkw_ct_def, other_srkw.sample(diff_srkw)])
tkw_ct_def = pl.concat([tkw_ct_def, other_tkw.sample(diff_tkw)])

def_df = pl.concat([srkw_ct_def, tkw_ct_def])
def_df.to_csv('./beats_test_set.csv')

