import pandas as pd
import os

# Change this to your actual workbook filename
workbook = "figures_WB.xlsx"

sheets = pd.read_excel(workbook, sheet_name=None)
for name, df in sheets.items():
    safe = "".join(c if c not in '/\\:*?"<>|' else "_" for c in name)
    df.to_csv(f"{safe}.csv", index=False)
    print(f"Wrote {safe}.csv")

print(f"\nDone. {len(sheets)} sheets exported.")
