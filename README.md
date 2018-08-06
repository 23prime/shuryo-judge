# Judgement Machine of ShuRyo

Judging whether you can ShuRyo or not.

- :exclamation: Not judging you can write ShuRon or not.

## Usage

- Download `seiseki.csv` from [Twins](https://twins.tsukuba.ac.jp/campusweb/).
- Type next.

```
$ stack exec shuryo-judge ./hoge.csv
```

- If completion requirement is changed, edit `src/Rules.hs`.


## About CSV data

| Row | Contents   |
|-----|------------|
|  0  | 学籍番号   |
|  1  | 氏名       |
|  2  | 科目番号   |
|  3  | 科目コード |
|  4  | 科目名     |
|  5  | 単位数     |
|  6  | 総合評価   |
|  7  | 認定年度   |
|  8  | 科目区分   |
|  9  | 認定学期   |

- Now, ignoring "科目コード" and judging from "科目番号".
- English ver. has different specification from Japanese, so now not supported.
- If specification of CSV changed, then edit `mkCredit` in `src/Rules.hs`.
