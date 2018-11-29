# Judgement Machine of Completion

Judging whether you can complete graduate school or not.

Running by [Yesod](https://www.yesodweb.com/), on [Heroku](https://jp.heroku.com/).

:warning: Not judging you can write paper or not.

## Usage

Go to [Here](https://shuryo-judge.herokuapp.com/).

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

## ToDo

- [ ] Make a design decent.
- [ ] Separate the result page.